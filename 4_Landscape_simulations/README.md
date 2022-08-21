# Landscape_simulation

How to run your simulations:

1. install Rust on your computer: https://www.rust-lang.org/tools/install
2. get the code and build it:

```bash
git clone https://github.com/mneyret/Landscape_simulation
cd Landscape_simulation
cargo build --release
```

3. Use the input files from the input folder. The input file for default parameters is "input_env_corr_regionFALSE_scale_withinTRUE_cropconstrained.toml"

4. run the code (replace <YOUR/INPUT/FILE.toml> by your actual file)

```bash
export RAYON_NUM_THREADS=<number of CPU cores to use>
# Examples
cargo run --release -- <YOUR/INPUT/FILE.toml>
# Actual commands
cargo run --release -- inputs/input_env_corr_regionFALSE_scale_withinTRUE_cropconstrained.toml
cargo run --release -- inputs/input_env_corr_regionTRUE_scale_withinTRUE_cropconstrained.toml
cargo run --release -- inputs/input_env_corr_regionFALSE_scale_withinFALSE_cropconstrained.toml
cargo run --release -- inputs/input_env_corr_regionFALSE_scale_withinTRUE.toml
cargo run --release -- inputs/input_env_corr_regionTRUE_scale_withinTRUE.toml
cargo run --release -- inputs/input__regionFALSE_scale_withinTRUE_cropconstrained.toml
# or
./target/release/landscape_simulation <YOUR/INPUT/FILE.toml>
```

