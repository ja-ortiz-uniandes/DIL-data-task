# Development Impact Lab at the University of Chicago

Repository URL: [https://github.com/ja-ortiz-uniandes/DIL-data-task](https://github.com/ja-ortiz-uniandes/DIL-data-task)

## About

This repository contains the solution to the Development Impact Lab Predoctoral Research Professional Data Task.

**Important Notice:**

- This repository contains no confidential data, datasets, or proprietary instructions.
- The repository will be closed 21 days after submission.

## Project Structure

- `Alejandro Ortiz - data task report.qmd` - Quarto document used to produce the PDF report.
- `Scripts/` - R scripts with analysis functions and main processing code.
- `Output/` - Generated figures and results. Non confidential data.
- `support/` - LaTeX templates, bibliography, and formatting files.
- `renv.lock` - R environment lock file for reproducibility.
- `config.yml` - Configuration file.

## Replication Instructions

The preferred way to reproduce this code is to clone the repo directly from GitHub.

### Prerequisites

1. **R and RStudio**: Ensure you have R (version 4.5+) and RStudio (suggested) installed.
   1. Please ensure you have `renv` installed in the local global environment.
2. **Fira Code Font**: Install the Fira Code font to ensure proper PDF output formatting. Download from [https://github.com/tonsky/FiraCode](h.ttps://github.com/tonsky/FiraCode).
3. **LaTeX Distribution**: Install a LaTeX distribution (e.g., MiKTeX on Windows, MacTeX on macOS, or TeX Live on Linux) for PDF generation. Ensure that your distribution supports lualatex, kableExtra, and tinytable.

### Setup Environment

1. Clone the repository:

   Terminal:

   ```bash
   git clone https://github.com/ja-ortiz-uniandes/DIL-data-task
   cd DIL-data-task
   ```

2. Open the project in RStudio by opening the `.Rproj` file.

3. Restore the R environment using `renv`:

   R console:

   ```r
   renv::restore()
   ```

   Follow the prompts to "Restore the project from the lockfile." This will install all required R packages as specified in the `renv.lock` file.

4. Configure data paths:

   Terminal:

   ```bash
   # Create a copy of the configuration template and remove the .template extension
   cp config.yml.template config.yml
   ```

   Then edit `config.yml` to replace the placeholder variables with your actual data paths:

   - Replace `PATH_TO_INPUT_DATA_FOLDER_HERE` with the path to your input data folder.
   - Replace `PATH_TO_OUTPUT_DATA_FOLDER_HERE` with the path to your output data folder.

   Make sure all directory paths use `/` instead of `\`.

   **Security Note:** The `config.yml` file is excluded from version control to protect machine-specific and potentially sensitive path information. This ensures that local configurations remain private and are not committed to the repository. It also allows you to work with confidential data stored in secure or encrypted environments that may differ from the repository’s location.

After completing all the steps above you should now be able to reproduce all products.

### Generate the Report

1. Open the `.qmd` file in RStudio.
2. Render the document by click the "Render" button in RStudio.

The rendered PDF will be saved as in the project's home directory.

## Output

The main deliverables are:

- A PDF report. Found in the home directory.
- The code used in the task. Found in `./Scripts/`

## Technical Notes

- The document uses LuaLaTeX as the PDF engine for enhanced font support.
- Fira Code font is used for monospaced text and code blocks.
- The analysis leverages the `renv` package for reproducible dependency management.
- Code execution is hidden by default in the final PDF (`echo: false`).

## Dependencies

See renv lock file.

## Contact

For questions about this analysis, please refer to the repository issues or contact information provided in the original task instructions.
