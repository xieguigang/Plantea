let plant_motif_search = function(seqfile, plant_code, outdir = "./", n_threads = 8) {
    GCModeller::scan_motifs(db = locate_meme_dir(plant_code), seqs = seqfile, 
                            identities_cutoff = 0.8,
                            minW = 0.85,
                            top = 3,
                            permutation = 2500,             
                            workdir = outdir, 
                            n_threads = n_threads);
}