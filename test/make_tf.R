require(Plantea);

imports "annotation.workflow" from "seqtoolkit";

let tf_family = motif_tfinfo("Tae");
let tf = read_m8("F:\datapool\20260312\20260322\Lanmai.txt") |> assign_tffamily( TFdb = tf_family);

print(tf);