# Rust template listing

# Include the lmcp-derive proc-macro crate
ONCE            rust/lmcp-derive/Cargo.toml lmcp-derive/Cargo.toml
ONCE            rust/lmcp-derive/lib.rs     lmcp-derive/lib.rs

# Include the top-level library
ONCE            rust/Cargo.toml             Cargo.toml
ONCE            rust/src/lib.rs             src/lib.rs
ONCE            rust/src/avtas/mod.rs       src/avtas/mod.rs
ONCE            rust/src/avtas/lmcp.rs      src/avtas/lmcp.rs

# Test binary
ONCE            rust/src/main.rs            src/main.rs

# Subsumes per-MDM file, so we can flesh out the Rust module structure properly
PER_NS_SUBDIR   rust/mdm-mod.rs             src/-<namespace_subdir>-/mod.rs

PER_STRUCT      rust/struct.rs              src/-<series_dir>-/-<datatype_snake_name>-.rs
PER_ENUM        rust/enum.rs                src/-<series_dir>-/-<enum_snake_name>-.rs
