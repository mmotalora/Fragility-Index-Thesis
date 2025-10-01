.PHONY: all setup ingest fi model figs repro

all: setup ingest fi model figs repro

setup:
	Rscript scripts/00_setup.R

ingest:
	Rscript scripts/01_data_ingest_and_clean.R

fi:
	Rscript scripts/02_compute_fi_fq.R

model:
	Rscript scripts/03_model_negative_binomial.R

figs:
	Rscript scripts/04_figures_and_tables.R

repro:
	Rscript scripts/05_reproducibility_checks.R