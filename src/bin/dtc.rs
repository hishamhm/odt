mod dtc_main;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    dtc_main::dtc_main(std::env::args_os())
}
