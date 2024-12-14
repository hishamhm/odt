mod dtc_main;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // When invoked as `cargo dtc`, we get an extra `dtc` as the first argument.
    // Compensate by shifting away argv[0].
    dtc_main::dtc_main(std::env::args_os().skip(1))
}
