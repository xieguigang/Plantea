const PlantTFDB_motifs = function() {
    system.file("data/PlantTFDB/PlantTFDB_TF_binding_motifs_from_experiments.xml",
        package = "Plantea"
    )
    |> Plantea::load_motifdb()
    ;
}