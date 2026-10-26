export const isBrowser = () => typeof document !== "undefined";

export const requireElementById = (id) => () => {
  const element = document.getElementById(id);
  if (element === null) {
    throw new Error(`Could not find an element with id "${id}".`);
  }
  return element;
};

export const createElement = (tag) => () => document.createElement(tag);

export const createTextNode = (value) => () => document.createTextNode(value);

export const appendChild = (parent) => (child) => () => {
  parent.appendChild(child);
};

export const clearChildren = (element) => () => {
  while (element.firstChild !== null) {
    element.removeChild(element.firstChild);
  }
};

export const setAttribute = (element) => (name) => (value) => () => {
  element.setAttribute(name, value);
};

export const removeAttribute = (element) => (name) => () => {
  element.removeAttribute(name);
};

export const setProperty = (element) => (name) => (value) => () => {
  element[name] = value;
};

export const addEventListener = (element) => (eventName) => (handler) => () => {
  element.addEventListener(eventName, (event) => {
    handler(event)();
  });
};

export const eventValue = (event) => {
  const target = event.target;
  return target !== null && typeof target === "object" && "value" in target
    ? String(target.value)
    : "";
};

export const eventChecked = (event) => {
  const target = event.target;
  return target !== null && typeof target === "object" && "checked" in target
    ? Boolean(target.checked)
    : false;
};

export const eventKey = (event) =>
  typeof event.key === "string" ? event.key : "";

export const eventShiftKey = (event) => Boolean(event.shiftKey);

export const eventCtrlKey = (event) => Boolean(event.ctrlKey);

export const eventAltKey = (event) => Boolean(event.altKey);

export const eventMetaKey = (event) => Boolean(event.metaKey);
