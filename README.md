# Parties' ideological cores vs peripheries
Replication data and scripts for BJPS article “Parties’ ideological cores and peripheries: Examining how parties balance adaptation and continuity in their manifestos”

---

### `1_get_marpor_manifestos.R`
**Purpose:**
Downloads and processes the MARPOR (Manifesto Project) corpus into a quasi-sentence–level tidy dataset suitable for classification.

**Output:**
- `marpor_corpus_quasi.RDS`

---

### `2_classify_quasi_berta.py`
**Purpose:**
Classifies quasi-sentences using a the XLM-RoBERTa-based model `manifestoberta` (see here for documentation: https://doi.org/10.25522/manifesto.manifestoberta.56topics.sentence.2023.1.1).

**Inputs:**  
- `marpor_corpus_quasi.RDS`

**Related files:**
- `classify_quasi.slurm` — SLURM batch script used for running the classifier on high-performance computing cluster at the University of Innsbruck (see here: https://www.uibk.ac.at/zid/systeme/hpc-systeme).

**Outputs:**  
- `marpor_corpus_quasi_classified.RDS` — final merged output of classified quasi-sentences

**Intermediate batch files:**  
These files provide access to `marpor_corpus_quasi_classified.RDS` in three separate batches that are later merged in the analysis stage.
- `marpor_quasi_classified_a.RDS` — Text of quasi-sentences with MARPOR variables and unique ID (`1:nrow()`).
- `marpor_quasi_classified_b.RDS` — Classified labels (top 1–5) and probabilities with matching IDs.
- `marpor_quasi_classified_c.RDS` — Classified labels (top 6–10) and probabilities with matching IDs.

---

### `3_analyze_quasi_classifications.R`
**Purpose:**  
Merges the classified outputs and reproduces all statistical models and figures presented in the article.

**Inputs:**  
- `marpor_quasi_classified_a.RDS`  
- `marpor_quasi_classified_b.RDS`  
- `marpor_quasi_classified_c.RDS`  
- `pg_missing_coded.xlsx` — Manual coding of parties' incumbency status where ParlGov information is missing.

**Outputs:**  
- Statistical models and figures as reported in the BJPS article.

---

## Dependencies
The analyses were conducted using **R** and **Python**.
- **R version:** `4.3.3`
  - Main required packages: `tidyverse`, `readxl`, `manifestoR`, `ggeffects`, `fixest`, `knitr`, `tidytext`, `countrycode`.
- **Python version:** `3.13`
  - Main required packages: `transformers`, `torch`, `pandas`, `pyreadr`.

---

## Citation
> Werner, Annika \& Habersack, Fabian (forthcoming). Parties’ ideological cores and peripheries: Examining how parties balance adaptation and continuity in their manifestos,
*British Journal of Political Science,* OSF Preprint: https://doi.org/10.31235/osf.io/pjes3_v1
