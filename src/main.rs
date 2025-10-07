use ship::cli;

fn main() -> anyhow::Result<()> {
    cli::cli()?;
    Ok(())
}
