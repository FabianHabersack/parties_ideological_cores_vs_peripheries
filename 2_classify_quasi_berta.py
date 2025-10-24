# -*- coding: utf-8 -*-
"""

# =========================================================================== #
# Replication Python script for: "Parties' ideological cores and peripheries" #
# =========================================================================== #

# Last updated: 24-10-2025

"""

# Import packages
import pandas as pd
import pyreadr
import torch
from transformers import AutoModelForSequenceClassification, AutoTokenizer

# Load the model and tokenizer
model = AutoModelForSequenceClassification.from_pretrained("manifesto-project/manifestoberta-xlm-roberta-56policy-topics-sentence-2023-1-1")
tokenizer = AutoTokenizer.from_pretrained("xlm-roberta-large")

# Path to the input file and output file
input_file =  "/scratch/cXXXXXXX/marpor_corpus_quasi.RDS"
output_file = "/scratch/cXXXXXXX/marpor_corpus_quasi_classified.RDS"

# Read the manifesto data
manifestos = pyreadr.read_r(input_file)
manifestos_df = manifestos[None]

# Prepare input for tokenization
sentences = manifestos_df['text'].tolist()
encoded_inputs = tokenizer(sentences, padding=True, truncation=True, max_length=200, return_tensors="pt")

# Process logits in batches
batch_size = 100
n = len(manifestos_df)
logits = []

for start_idx in range(0, n, batch_size):
    end_idx = start_idx + batch_size
    batch_inputs = {key: val[start_idx:end_idx] for key, val in encoded_inputs.items()}
    with torch.no_grad():
        outputs = model(**batch_inputs).logits
    logits.append(outputs)

logits = torch.cat(logits, dim=0)

# Convert logits to probabilities
probabilities = torch.softmax(logits, dim=1)

# Extract top 10 labels and scores
top10 = torch.topk(probabilities, 10, dim=1)
top10_labels = [[model.config.id2label[idx.item()] for idx in top10.indices[i]] for i in range(top10.indices.size(0))]
top10_scores = top10.values.tolist()

# Add top predictions to the DataFrame
for i in range(10):
    manifestos_df[f'top{i+1}_label'] = [labels[i] for labels in top10_labels]
    manifestos_df[f'top{i+1}_score'] = [scores[i] for scores in top10_scores]

# Save the classified data as RDS
pyreadr.write_rds(output_file, manifestos_df)