imports "motif_tool" from "TRNtoolkit";

const PlantTFDB_motifs = function() {
    system.file("data/PlantTFDB/PlantTFDB_TF_binding_motifs_from_experiments.xml",
        package = "Plantea"
    )
    |> Plantea::load_motifdb()
    ;
}

const open_meme = function(code) {
    motif_tool::open_meme_dir(locate_meme_dir(code));
}

const locate_meme_dir = function(code) {
    let data_dir = file.path(@datadir, code);
    let meme_dir = file.path(data_dir, `${code}_TF_binding_motifs_individual`);

    return(meme_dir);
}