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
                    let frame = document.createElement("iframe");
                    frame.setAttribute("class", "eq"+queries.length);

                    if (node.dataset.source) queries.push("source=" + encodeURIComponent(node.dataset.source));
                    frame.src = "../index.html?" + queries.join("&");
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
    let menu = document.getElementsByTagName("nav");
    let main = document.getElementsByTagName("main");
    let menuIcon = document.getElementById("menu");
    if (menu.length == 0 || main.length == 0 || !menuIcon) return;
    if (menu[0].className == "closed") {
        menu[0].className = "";
        main[0].className = "";
        menuIcon.className = "";
    } else {
        menu[0].className = "closed";
        main[0].className = "closed";
        menuIcon.className = "closed";
    }
}