use std::slice;
use std::ffi::{CString, CStr};
use std::panic;

use cty::{c_char, c_int, c_float, c_void};

use wotw_seedgen::settings::UniverseSettings;
use wotw_seedgen::logic;
use wotw_seedgen::Inventory;
use wotw_seedgen::item::{Item, Skill, Teleporter, Shard, Resource};
use wotw_seedgen::uber_state::UberStateComparator;
use wotw_seedgen::world::graph::Node;

#[repr(C)]
#[derive(Clone, Copy)]
pub struct RawVec<T> {
    data: *const T,
    len: c_int,
}

#[repr(C)]
pub struct ReachCheckRequest {
    health: c_float,
    energy: c_float,
    spirit_light: c_int,
    gorlek_ore: c_int,
    keystone: c_int,
    shard_slot: c_int,
    clean_water: c_char,
    skills: RawVec<c_int>,
    teleporters: RawVec<c_int>,
    shards: RawVec<c_int>,
    set_nodes: RawVec<*const c_char>,
}

#[repr(C)]
pub struct SeedgenInput {
    areas: *const c_char,
    locations: *const c_char,
    states: *const c_char,
    seed_file: *const c_char,
}

pub type Data = *const c_void;

trait HandlesErrors {
    fn push_error(&self, data: Data, string: *const c_char);
}

#[repr(C)]
pub struct ReachCheckFunctions {
    push_error: extern fn(data: Data, string: *const c_char),
    push_reached: extern fn(data: Data, string: *const c_char),
}

impl ReachCheckFunctions {
    #[inline]
    fn push_reached(&self, data: Data, string: *const c_char) {
        (self.push_reached)(data, string)
    }
}

impl HandlesErrors for ReachCheckFunctions {
    #[inline]
    fn push_error(&self, data: Data, string: *const c_char) {
        (self.push_error)(data, string)
    }
}

#[repr(C)]
pub struct GetStatesFunctions {
    push_error: extern fn(data: Data, string: *const c_char),
    push_state: extern fn(data: Data, name: *const c_char, uber_group: i32, uber_state: i32, value: i32, op: i32),
}

impl GetStatesFunctions {
    #[inline]
    fn push_state(&self, data: Data, name: *const c_char, uber_group: i32, uber_state: i32, value: i32, op: i32) {
        (self.push_state)(data, name, uber_group, uber_state, value, op)
    }
}

impl HandlesErrors for GetStatesFunctions {
    #[inline]
    fn push_error(&self, data: Data, string: *const c_char) {
        (self.push_error)(data, string)
    }
}

#[repr(C)]
pub enum Status {
    Success,
    Error,
    Panic,
}

fn handle_panics<F: FnOnce() -> Result<(), String> + panic::UnwindSafe>(data: Data, functions: &dyn HandlesErrors, f: F) -> Status {
    match panic::catch_unwind(f) {
        Ok(result) => match result {
            Ok(()) => Status::Success,
            Err(err) => {
                let error_message = CString::new(err).unwrap_or_else(|_| CString::new("Failed to convert error message").unwrap());
                functions.push_error(data, error_message.into_raw());
                Status::Error
            }
        },
        Err(_) => Status::Panic,
    }
}

unsafe fn convert_c_str<'a>(ptr: *const c_char) -> Result<&'a str, String> {
    CStr::from_ptr(ptr).to_str().map_err(|err| format!("invalid areas file: {err}"))
}

unsafe fn convert_c_vec<'a, T>(raw_vec: RawVec<T>) -> &'a [T] {
    if raw_vec.len == 0 {
        return &[];
    }

    slice::from_raw_parts(raw_vec.data, raw_vec.len as usize)
}

#[no_mangle]
pub extern "C" fn reach_check(input: &SeedgenInput, reach_check_request: &ReachCheckRequest, data: Data, functions: &ReachCheckFunctions) -> Status {
    handle_panics(data, functions, || {
        unsafe {
            let inventory = create_inventory(&reach_check_request)?;

            let set_nodes = convert_c_vec(reach_check_request.set_nodes).into_iter()
                .map(|ptr| convert_c_str(*ptr)).collect::<Result<Vec<_>, _>>()?
                .into_iter().map(str::to_owned).collect::<Vec<_>>();

            let areas = convert_c_str(input.areas)?;
            let locations = convert_c_str(input.locations)?;
            let states = convert_c_str(input.states)?;
            let seed_file = convert_c_str(input.seed_file)?;
            let universe_settings = UniverseSettings::from_seed(seed_file).unwrap_or_else(|| Err("No settings found in seed".into()))?;
            let graph = logic::parse_logic(areas, locations, states, &universe_settings, false)?;

            let reached = wotw_seedgen::reach_check(inventory, &graph, seed_file, &set_nodes)?;

            for node in reached {
                let identifier = CString::new(node.identifier()).map_err(|err| format!("failed to convert node identifier: {err}"))?;
                functions.push_reached(data, identifier.into_raw());
            }
        }
        Ok(())
    })
}

#[no_mangle]
pub extern "C" fn get_states(input: &SeedgenInput, data: Data, functions: &GetStatesFunctions) -> Status {
    handle_panics(data, functions, || {
        unsafe {
            let areas = convert_c_str(input.areas)?;
            let locations = convert_c_str(input.locations)?;
            let states = convert_c_str(input.states)?;
            let seed_file = convert_c_str(input.seed_file)?;
            let universe_settings = UniverseSettings::from_seed(seed_file).unwrap_or_else(|| Err("No settings found in seed".into()))?;
            let graph = logic::parse_logic(areas, locations, states, &universe_settings, false)?;

            for node in graph.nodes {
                match node {
                    Node::State(state) => {
                        if state.trigger.is_none() {
                            continue;
                        }

                        let identifier = CString::new(state.identifier).map_err(|err| format!("failed to convert state identifier: {err}"))?;
                        let trigger = state.trigger.unwrap();

                        if let Some(condition) = trigger.condition {
                            functions.push_state(
                                data,
                                identifier.into_raw(),
                                trigger.identifier.uber_group as i32,
                                trigger.identifier.uber_id as i32,
                                condition.value.to_owned() as i32,
                                match &condition.comparator {
                                    UberStateComparator::Equals => 2,
                                    UberStateComparator::Greater => 4,
                                    UberStateComparator::GreaterOrEquals => 0,
                                    UberStateComparator::Less => 5,
                                    UberStateComparator::LessOrEquals => 1,
                                }
                            );
                        } else {
                            functions.push_state(
                                data,
                                identifier.into_raw(),
                                trigger.identifier.uber_group as i32,
                                trigger.identifier.uber_id as i32,
                                1,
                                2,  // OP = Equal to
                            );
                        }
                    }
                    _ => {}
                }
            }
        }
        Ok(())
    })
}

unsafe fn create_inventory(reach_check_request: &ReachCheckRequest) -> Result<Inventory, String> {
    let mut inventory = Inventory::default();

    inventory.grant(Item::Resource(Resource::HealthFragment), (reach_check_request.health / 5.0) as u32);
    inventory.grant(Item::Resource(Resource::EnergyFragment), (reach_check_request.energy * 2.0) as u32);
    inventory.grant(Item::SpiritLight(1), reach_check_request.spirit_light.try_into().map_err(|_| format!("invalid spirit light amount {}", reach_check_request.spirit_light))?);
    inventory.grant(Item::Resource(Resource::GorlekOre), reach_check_request.gorlek_ore.try_into().map_err(|_| format!("invalid gorlek ore amount {}", reach_check_request.gorlek_ore))?);
    inventory.grant(Item::Resource(Resource::Keystone), reach_check_request.keystone.try_into().map_err(|_| format!("invalid keystone amount {}", reach_check_request.keystone))?);
    inventory.grant(Item::Resource(Resource::ShardSlot), reach_check_request.shard_slot.try_into().map_err(|_| format!("invalid shard slot amount {}", reach_check_request.shard_slot))?);

    if reach_check_request.clean_water == 1 {
        inventory.grant(Item::Water, 1);
    }

    for skill in convert_c_vec(reach_check_request.skills) {
        inventory.grant(Item::Skill(Skill::try_from(*skill as u8).map_err(|_| format!("Invalid skill id {skill}"))?), 1);
    }
    for teleporter in convert_c_vec(reach_check_request.teleporters) {
        inventory.grant(Item::Teleporter(Teleporter::try_from(*teleporter as u8).map_err(|_| format!("Invalid teleporter id {teleporter}"))?), 1);
    }
    for shard in convert_c_vec(reach_check_request.shards) {
        inventory.grant(Item::Shard(Shard::try_from(*shard as u8).map_err(|_| format!("Invalid shard id {shard}"))?), 1);
    }

    Ok(inventory)
}
