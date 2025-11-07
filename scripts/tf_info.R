require(GCModeller);
require(Plantea);

imports "bioseq.fasta" from "seqtoolkit";

read.fasta("M:\project\20251010-wheat\20251105\LargePanicleDevelopment\blastp\TF\PlantTFDB-all_TF_pep.fas")
|> extract_tf_info()
|> write.csv("G:\Plantea\data\PlantTFDB\TF.csv")
;