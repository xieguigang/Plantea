imports "bioseq.fasta" from "seqtoolkit";

const motif_tfinfo = function(code) {
    let data_dir = file.path(@datadir, code);
    let seqfile = file.path(data_dir,`${code}_pep.fas`);
    let tf_seqs = read.fasta(seqfile);

    extract_tf_info(tf_seqs);
}