
document.addEventListener("copy", (event) => {
  const anchorNode = document.getSelection().anchorNode;
  if (anchorNode instanceof HTMLElement &&
    anchorNode.classList.contains("selectize-input")) {
    const items = Array.from(anchorNode.getElementsByClassName("item active"));
    const string = items.map(i => i.innerText).join(", ");
    event.clipboardData.setData("text/plain", string);
    event.preventDefault();
  }
});
