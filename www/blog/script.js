function run() {
    // Set callbacks on try-it-out
    for (let node of document.getElementsByClassName("try-it-out")) {
        let removalTime = null;
        let button = document.createElement("a");
        button.innerHTML = "<h3>&gt; Try it Out! (Click to expand)</h3>"
        button.addEventListener("click", () => {
            if (node.className == "try-it-out") {
                node.className = "try-it-out expand";
                removalTime = undefined;
                if (node.lastChild.nodeName != "IFRAME") {
                    let queries = JSON.parse(node.dataset.target).map((s) => "eq=" + encodeURIComponent(s))
                    if (node.dataset.source) queries.push("source=" + encodeURIComponent(node.dataset.source));
                    let frame = document.createElement("iframe");
                    frame.src = "../index.html?" + queries.join("&");
                    frame.setAttribute("class", "eq"+queries.length);
                    node.insertAdjacentElement("beforeend",frame);
                }
            } else {
                node.className = "try-it-out"
                removalTime = Date.now() + 1000;
                setTimeout(() => {
                    if (removalTime && removalTime < Date.now()) {
                        node.removeChild(node.lastChild);
                    }
                },1200);
            }
        });
        node.insertAdjacentElement("afterbegin", button);
    }
}
function toggleMenu() {
    console.log("yo");
    let menu = document.getElementsByTagName("nav");
    if (!menu) return;
    menu[0].className = menu[0].className == "closed" ? "" : "closed";
}