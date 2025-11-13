require(http);

let page = toString(requests.get("https://planttfdb.gao-lab.org/download.php"));

setwd(relative_work());

writeLines(page, con = "download.html");