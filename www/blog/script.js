function run() {
    // Update --n variable in css & prepare anchors
    let index = 0
    for (let node of document.getElementsByTagName("h2")) {
        let ref = document.createElement("a");
        ref.innerHTML = node.innerHTML;
        ref.tabIndex = 0;
        let id = node.innerHTML.replace(/\W+/g, '-').toLowerCase();
        ref.href = "#" + id;
        node.replaceChildren(ref);
        node.style.setProperty('--n', index.toString());

        let anchor = document.createElement("a");
        anchor.className = "anchor"
        anchor.id = id;
        anchor.style.setProperty('--n', index.toString());
        node.insertAdjacentElement("beforebegin", anchor);
        index++;
    }
    // Set callbacks on try-it-out
    for (let node of document.getElementsByClassName("try-it-out")) {
        let removalTime = null;
        let button = document.createElement("h3");
        button.innerHTML = "> Try it Out!"
        button.addEventListener("click", () => {
            if (node.className == "try-it-out") {
                node.className = "try-it-out expand";
                removalTime = undefined;
                if (node.lastChild.nodeName != "IFRAME") {
                    let queries = JSON.parse(node.dataset.target).map((s) => "eq=" + s)
                    if (node.dataset.source) queries.push("source=" + node.dataset.source);
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
    // Add title
    let title = document.createElement("div");
    title.innerHTML = "\
      <h1><a href=\"index.html\">Algebra Explorer</a></h1>\
      <p>A tool that allows people to explore both steps and rules of algebra</p>\
      <p>The source code can be found on\
        <a href=\"https://github.com/Dreksh/algebra-explorer\">https://github.com/Dreksh/algebra-explorer</a>\
      </p>\
    ";
    title.id = "title";
    document.body.insertAdjacentElement("afterbegin", title);
}