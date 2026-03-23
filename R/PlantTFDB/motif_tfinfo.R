imports "bioseq.fasta" from "seqtoolkit";

const motif_tfinfo = function(code) {
    extract_tf_info(motif_tfseq(code));
}

#' Load internal TF sequence dataset
#' 
#' @return a fasta sequence collection.
#' 
const motif_tfseq = function(code) {
    let data_dir = file.path(@datadir, code);
    let seqfile = file.path(data_dir,`${code}_pep.fas`);
    let tf_seqs = read.fasta(seqfile);

    return(tf_seqs);
}