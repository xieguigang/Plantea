imports "motif_tool" from "TRNtoolkit";

const load_motif_links = function(code = NULL, data_frame = FALSE) {
    if (is.null(code)) {
        load_global_links(data_frame);
    } else {
        load_taxonomy_links(code, data_frame);
    }
}

const load_global_links = function(data_frame = FALSE) {
    let resource = file.path(@datadir, "/PlantTFDB/PlantTFDB_TF_binding_motifs_from_experiments_information.txt");
    let clr_df = load.csv(resource, type = "motif_link", tsv = TRUE);

    if (data_frame) {
        as.data.frame(clr_df);
    } else {
        return(clr_df);
    }
}

const load_taxonomy_links = function(code, data_frame = FALSE) {
    let meme_motifs = list.files( locate_meme_dir(code), pattern = "*.meme");
    meme_motifs = as.list(meme_motifs, names = basename(meme_motifs));
    meme_motifs = lapply(tqdm(meme_motifs), function(path) {
        let motifs = read_meme(path);
        let t = strsplit([motifs]::name,  drop1 =FALSE);

        data.frame(
            # TRAES3BF107400040CFD_g -> TRAES3BF107400040CFD
            gene_id = gsub( t@{2},"_g",""), 
            matrix_id = t@{3}
        );
    });

    meme_motifs <- bind_rows(meme_motifs);

    if (!data_frame) {
        # cast clr object
        as.motif_links(
            matrix_id = meme_motifs$matrix_id,
            tf_id     = meme_motifs$gene_id
        );
    } else {
        return(meme_motifs);
    }
}