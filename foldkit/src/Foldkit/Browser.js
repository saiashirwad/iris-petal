export const requireElementById = (id) => () => {
  const element = document.getElementById(id);
  if (element === null) {
    throw new Error(`Could not find an element with id "${id}".`);
  }
  return element;
};

export const createElement = (tag) => () => document.createElement(tag);

export const createTextNode = (value) => () => document.createTextNode(value);

export const childNodes = (element) => () => Array.from(element.childNodes);

export const insertAt = (parent) => (child) => (index) => () => {
  const current = parent.childNodes[index] ?? null;
  if (current !== child) {
    parent.insertBefore(child, current);
  }
};

export const appendChild = (parent) => (child) => () => {
  parent.appendChild(child);
};

const disconnectTree = (node) => {
  if (node.__foldkitIntersectionObserver !== undefined) {
    node.__foldkitIntersectionObserver.disconnect();
    delete node.__foldkitIntersectionObserver;
  }
  for (const child of node.children ?? []) {
    disconnectTree(child);
  }
};

export const removeChild = (parent) => (child) => () => {
  disconnectTree(child);
  parent.removeChild(child);
};

export const replaceChild = (parent) => (next) => (previous) => () => {
  disconnectTree(previous);
  parent.replaceChild(next, previous);
};

export const clearChildren = (element) => () => {
  while (element.firstChild !== null) {
    const child = element.firstChild;
    disconnectTree(child);
    element.removeChild(child);
  }
};

export const setAttribute = (element) => (name) => (value) => () => {
  element.setAttribute(name, value);
};

export const removeAttribute = (element) => (name) => () => {
  element.removeAttribute(name);
};

export const setValue = (element) => (value) => () => {
  if (element.value !== value) {
    element.value = value;
  }
};

export const setNodeValue = (node) => (value) => () => {
  node.nodeValue = value;
};

const listenersKey = "__foldkitListeners";

export const setEventListener = (element) => (eventName) => (handler) => () => {
  if (element[listenersKey] === undefined) {
    element[listenersKey] = {};
  }
  const previous = element[listenersKey][eventName];
  if (previous !== undefined) {
    element.removeEventListener(eventName, previous);
  }
  const listener = (event) => {
    handler(event)();
  };
  element[listenersKey][eventName] = listener;
  element.addEventListener(eventName, listener);
};

export const removeEventListener = (element) => (eventName) => () => {
  const listeners = element[listenersKey];
  if (listeners === undefined) return;
  const listener = listeners[eventName];
  if (listener !== undefined) {
    element.removeEventListener(eventName, listener);
    delete listeners[eventName];
  }
};

export const setVisibleHandler = (element) => (handler) => () => {
  element.__foldkitVisible = handler;
  if (element.__foldkitIntersectionObserver !== undefined) return;
  if (typeof IntersectionObserver === "undefined") {
    handler();
    return;
  }
  const observer = new IntersectionObserver(
    (entries) => {
      if (entries.some((entry) => entry.isIntersecting)) {
        element.__foldkitVisible?.();
      }
    },
    { rootMargin: "240px 0px" },
  );
  element.__foldkitIntersectionObserver = observer;
  observer.observe(element);
};

export const removeVisibleHandler = (element) => () => {
  element.__foldkitIntersectionObserver?.disconnect();
  delete element.__foldkitIntersectionObserver;
  delete element.__foldkitVisible;
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

export const eventKey = (event) => ({
  key: typeof event.key === "string" ? event.key : "",
  shiftKey: Boolean(event.shiftKey),
  ctrlKey: Boolean(event.ctrlKey),
  altKey: Boolean(event.altKey),
  metaKey: Boolean(event.metaKey),
});

export const captureFocus = () => {
  const element = document.activeElement;
  if (element === null || !("id" in element) || !element.id) {
    return { id: "", start: 0, end: 0 };
  }
  return {
    id: element.id,
    start: typeof element.selectionStart === "number" ? element.selectionStart : 0,
    end: typeof element.selectionEnd === "number" ? element.selectionEnd : 0,
  };
};

export const restoreFocus = (state) => () => {
  if (!state.id) return;
  const element = document.getElementById(state.id);
  if (element === null) return;
  element.focus();
  if (typeof element.setSelectionRange === "function") {
    element.setSelectionRange(state.start, state.end);
  }
};
