use std::slice;
use std::ffi::{CString, CStr};
use std::panic;

use cty::{c_char, c_int, c_float, c_void};

use wotw_seedgen::settings::UniverseSettings;
use wotw_seedgen::logic;
use wotw_seedgen::Inventory;
use wotw_seedgen::item::{Item, Skill, Teleporter, Shard, Resource};

#[repr(C)]
#[derive(Clone, Copy)]
pub struct RawVec<T> {
    data: *const T,
    len: c_int,
}

#[repr(C)]
pub struct Input {
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
    nodes: RawVec<*const c_char>,
    areas: *const c_char,
    locations: *const c_char,
    states: *const c_char,
    seed_file: *const c_char,
}
pub type Data = *const c_void;
#[repr(C)]
pub struct Functions {
    push_error: extern fn(data: Data, string: *const c_char),
    push_reached: extern fn(data: Data, string: *const c_char),
}
impl Functions {
    #[inline]
    fn push_error(&self, data: Data, string: *const c_char) {
        (self.push_error)(data, string)
    }
    #[inline]
    fn push_reached(&self, data: Data, string: *const c_char) {
        (self.push_reached)(data, string)
    }
}

#[repr(C)]
pub enum Status {
    Success,
    Error,
    Panic,
}

fn handle_panics<F: FnOnce() -> Result<(), String> + panic::UnwindSafe>(data: Data, functions: &Functions, f: F) -> Status {
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
    slice::from_raw_parts(raw_vec.data, raw_vec.len as usize)
}

#[no_mangle]
pub extern "C" fn reach_check(input: &Input, data: Data, functions: &Functions) -> Status {
    handle_panics(data, functions, || {
        unsafe {
            let inventory = create_inventory(&input)?;

            let nodes = convert_c_vec(input.nodes).into_iter()
                .map(|ptr| convert_c_str(*ptr)).collect::<Result<Vec<_>, _>>()?
                .into_iter().map(str::to_owned).collect::<Vec<_>>();

            let areas = convert_c_str(input.areas)?;
            let locations = convert_c_str(input.locations)?;
            let states = convert_c_str(input.states)?;
            let seed_file = convert_c_str(input.seed_file)?;
            let universe_settings = UniverseSettings::from_seed(seed_file).unwrap_or_else(|| Err("No settings found in seed".into()))?;
            let graph = logic::parse_logic(areas, locations, states, &universe_settings, false)?;

            let reached = wotw_seedgen::reach_check(inventory, &graph, seed_file, &nodes)?;

            for node in reached {
                let identifier = CString::new(node.identifier()).map_err(|err| format!("failed to convert node identifier: {err}"))?;
                functions.push_reached(data, identifier.into_raw());
            }
        }
        Ok(())
    })
}

unsafe fn create_inventory(input: &Input) -> Result<Inventory, String> {
    let mut inventory = Inventory::default();

    inventory.grant(Item::Resource(Resource::HealthFragment), (input.health / 5.0) as u32);
    inventory.grant(Item::Resource(Resource::EnergyFragment), (input.energy * 2.0) as u32);
    inventory.grant(Item::SpiritLight(1), input.spirit_light.try_into().map_err(|_| format!("invalid spirit light amount {}", input.spirit_light))?);
    inventory.grant(Item::Resource(Resource::GorlekOre), input.gorlek_ore.try_into().map_err(|_| format!("invalid gorlek ore amount {}", input.gorlek_ore))?);
    inventory.grant(Item::Resource(Resource::Keystone), input.keystone.try_into().map_err(|_| format!("invalid keystone amount {}", input.keystone))?);
    inventory.grant(Item::Resource(Resource::ShardSlot), input.shard_slot.try_into().map_err(|_| format!("invalid shard slot amount {}", input.shard_slot))?);

    if input.clean_water == 1 {
        inventory.grant(Item::Water, 1);
    }

    for skill in convert_c_vec(input.skills) {
        inventory.grant(Item::Skill(Skill::try_from(*skill as u8).map_err(|_| format!("Invalid skill id {skill}"))?), 1);
    }
    for teleporter in convert_c_vec(input.teleporters) {
        inventory.grant(Item::Teleporter(Teleporter::try_from(*teleporter as u8).map_err(|_| format!("Invalid teleporter id {teleporter}"))?), 1);
    }
    for shard in convert_c_vec(input.shards) {
        inventory.grant(Item::Shard(Shard::try_from(*shard as u8).map_err(|_| format!("Invalid shard id {shard}"))?), 1);
    }

    Ok(inventory)
}
