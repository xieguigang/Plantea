const load_motif_links = function(data_frame = FALSE) {
    let resource = system.file("data/PlantTFDB/PlantTFDB_TF_binding_motifs_from_experiments_information.txt",
             package = "Plantea");
    let clr_df = load.csv(resource, type = "motif_link", tsv = TRUE);

    if (data_frame) {
        as.data.frame(clr_df);
    } else {
        return(clr_df);
    }
}