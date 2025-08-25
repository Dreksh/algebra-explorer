function run() {
    // Update --n variable in css & prepare anchors
    let navbar = document.getElementsByTagName("nav");
    if (navbar.length != 0) {
        let bar = navbar[0];
        for (let node of document.getElementsByTagName("h2")) {
            let ref = document.createElement("a");
            ref.innerHTML = node.innerHTML;
            let id = node.innerHTML.replace(/\W+/g, '-').toLowerCase();
            ref.href = "#" + id;
            bar.insertAdjacentElement("beforeend", ref);

            let anchor = document.createElement("a");
            anchor.id = id;
            node.insertAdjacentElement("beforebegin", anchor);
        }
    }
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
    if (!menu) return;
    menu[0].className = menu[0].className == "closed" ? "" : "closed";
}