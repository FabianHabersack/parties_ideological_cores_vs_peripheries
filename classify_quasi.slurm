#!/bin/bash
#SBATCH --job-name=classify_quasi                      # Job name
#SBATCH --ntasks=1                                     # Number of tasks
#SBATCH --cpus-per-task=32                             # CPU cores per task
#SBATCH --mem=200G                                     # Memory allocation
#SBATCH --time=5-00:00:00                              # Maximum runtime
#SBATCH --output=/scratch/cXXXXXXX/quasi_output.log    # Output log
#SBATCH --error=/scratch/cXXXXXXX/quasi_error.log      # Error log
#SBATCH --mail-type=BEGIN,END,FAIL                     # Send email on job start, end, and failure
#SBATCH --mail-user=fabian.habersack@uibk.ac.at        # Email address for notifications

# Load necessary modules
module load python/3.8.13-anaconda-mpi-conda-2023.03   # Load Python 3.8
module load python/pytorch-2.0.1-conda-2023.10         # Load PyTorch

# Set PYTHONUSERBASE to SCRATCH to avoid using ~/.local
export PYTHONUSERBASE=/scratch/cXXXXXXX/python_packages
mkdir -p $PYTHONUSERBASE

# Install pyreadr, transformers, and openpyxl using pip (if needed)
pip install --user pyreadr transformers openpyxl

# Run the Python script from SCRATCH
python /scratch/cXXXXXXX/classify_quasi_berta.py

# Check for errors and send the error log via email if it exists
if [[ -s /scratch/cXXXXXXX/quasi_error.log ]]; then
  mail -s "Job classify_quasi encountered an error" fabian.habersack@uibk.ac.at < /scratch/cXXXXXXX/quasi_error.log
fi