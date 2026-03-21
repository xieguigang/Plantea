require(Plantea);

let db = open_meme("Tae");

print([db]::FamilyList);

let pwm = db |> load_motifs(name = "TRAES3BF001100080CFD_g");

for(let motif in pwm) {
    print(as.data.frame(motif));
}