/**
 * shadcn/ui-flavored primitives for the explorer, built as zero-dependency
 * vanilla DOM. These mirror the anatomy of shadcn's React components (Button
 * variants, Badge variants, lucide icons) so the explorer reads and looks like
 * a shadcn app without pulling in React, Tailwind, or Radix. The visual styling
 * lives in `styles.ts` under the same `lx-*` class names; this module only
 * builds the correctly-shaped, correctly-classed DOM.
 */

/** lucide icon path data (24x24 viewBox, stroke=currentColor). */
const ICON_PATHS = {
  play: '<polygon points="6 3 20 12 6 21 6 3"/>',
  "rotate-cw": '<path d="M21 12a9 9 0 1 1-3-6.7L21 8"/><path d="M21 3v5h-5"/>',
  check: '<path d="M20 6 9 17l-5-5"/>',
  "shield-check":
    '<path d="M20 13c0 5-3.5 7.5-7.66 8.95a1 1 0 0 1-.67-.01C7.5 20.5 4 18 4 13V6a1 1 0 0 1 1-1c2 0 4.5-1.2 6.24-2.72a1.17 1.17 0 0 1 1.52 0C14.51 3.81 17 5 19 5a1 1 0 0 1 1 1z"/><path d="m9 12 2 2 4-4"/>',
  braces:
    '<path d="M8 3H7a2 2 0 0 0-2 2v5a2 2 0 0 1-2 2 2 2 0 0 1 2 2v5c0 1.1.9 2 2 2h1"/><path d="M16 21h1a2 2 0 0 0 2-2v-5c0-1.1.9-2 2-2a2 2 0 0 1-2-2V5a2 2 0 0 0-2-2h-1"/>',
  search: '<circle cx="11" cy="11" r="8"/><path d="m21 21-4.3-4.3"/>',
  x: '<path d="M18 6 6 18"/><path d="m6 6 12 12"/>',
  plug: '<path d="M12 22v-5"/><path d="M9 8V2"/><path d="M15 8V2"/><path d="M18 8v5a4 4 0 0 1-4 4h-4a4 4 0 0 1-4-4V8Z"/>',
  download: '<path d="M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4"/><polyline points="7 10 12 15 17 10"/><line x1="12" x2="12" y1="15" y2="3"/>',
} as const;

export type IconName = keyof typeof ICON_PATHS;

/** A single lucide-style SVG string (used where an element isn't convenient, e.g. `innerHTML`). */
export function iconSvg(name: IconName): string {
  return (
    '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="none" stroke="currentColor" ' +
    'stroke-width="2" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true">' +
    ICON_PATHS[name] +
    "</svg>"
  );
}

/** A lucide icon as an inline element, sized by the surrounding `lx-ico` CSS. */
export function icon(name: IconName): HTMLElement {
  const span = document.createElement("span");
  span.className = "lx-ico";
  span.setAttribute("aria-hidden", "true");
  span.innerHTML = iconSvg(name);
  return span;
}

export type ButtonVariant = "default" | "primary" | "outline" | "ghost";

const VARIANT_CLASS: Record<ButtonVariant, string> = {
  default: "",
  primary: "lx-primary",
  outline: "lx-outline",
  ghost: "lx-ghost",
};

export interface ButtonOptions {
  readonly variant?: ButtonVariant;
  readonly icon?: IconName;
  readonly title?: string;
  readonly className?: string;
}

/** A shadcn-shaped `<button>`: optional leading lucide icon + a text label. */
export function button(label: string, opts: ButtonOptions = {}): HTMLButtonElement {
  const el = document.createElement("button");
  const variant = VARIANT_CLASS[opts.variant ?? "default"];
  el.className = [variant, opts.className].filter(Boolean).join(" ");
  if (opts.title) el.title = opts.title;
  if (opts.icon) el.appendChild(icon(opts.icon));
  el.appendChild(document.createTextNode(label));
  return el;
}
