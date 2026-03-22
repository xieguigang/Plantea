require(Plantea);

imports "bioseq.patterns" from "seqtoolkit";

let sites = read.scans("F:\datapool\20260312\20260322\Lanmai_upstream_500bp_vs_PlantTFDB_TF_binding_motifs_from_experiments_motif_scan.csv");

print(sites);