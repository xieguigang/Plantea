require(http);
require(Html);

let url  = relative_work("download.html"); # "https://planttfdb.gao-lab.org/download.php";
let page = toString(requests.get(url));

setwd(relative_work());

# writeLines(page, con = "download.html");

let tables = Html::tables(page, filter = TRUE, plain_text = FALSE);
let bind_motif = tables$bind_motif;

print(bind_motif, max.print = 13);

# for(let name in names(tables)) {
#     print(name);
#     print(tables[[name]]);
# }

for(let bind_data in tqdm(bind_motif$"Motif information")) {
    let url = `https://planttfdb.gao-lab.org/${Html::link(bind_data)}`;
    let infodata = toString(requests.get(url));
    let name = basename(url) |> gsub("_TF_binding_motifs_information");

    writeLines(infodata, con = 
        relative_work(`../data/PlantTFDB/bind_motif/${name}.txt`));
    sleep(1);
}