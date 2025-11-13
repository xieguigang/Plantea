require(http);
require(Html);

let url  = relative_work("download.html"); # "https://planttfdb.gao-lab.org/download.php";
let page = toString(requests.get(url));

setwd(relative_work());

# writeLines(page, con = "download.html");

let tables = Html::tables(page, filter = TRUE, plain_text = FALSE);

for(let name in names(tables)) {
    print(tables[[name]]);
}