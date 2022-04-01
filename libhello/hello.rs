#[macro_use]
extern crate cfg_if;

pub fn hello() -> &'static str {
    cfg_if! {
        if #[cfg(target_pointer_width = "32")] {
	      return "hello 32 bits!";
	} else {
	    return "probably 64 bits";
	}
    }
}
