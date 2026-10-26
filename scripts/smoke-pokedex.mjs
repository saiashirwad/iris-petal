import { chromium } from "playwright";

const baseUrl = process.env.POKEDEX_URL ?? "http://localhost:5173/";
const browser = await chromium.launch();

try {
  const page = await browser.newPage({ viewport: { width: 1280, height: 900 } });
  await page.goto(baseUrl, { waitUntil: "domcontentloaded" });

  await page.getByRole("button", { name: /bulbasaur/i }).waitFor({ timeout: 15000 });

  const search = page.getByRole("searchbox", { name: "Search Pokémon" });
  await search.fill("char");

  const focusedId = await page.evaluate(() => document.activeElement?.id ?? "");
  if (focusedId !== "search") {
    throw new Error(`focus left the search input after re-render: ${focusedId || "(none)"}`);
  }

  await page.getByRole("button", { name: /charmander/i }).waitFor();
  if ((await page.getByRole("button", { name: /bulbasaur/i }).count()) !== 0) {
    throw new Error("Bulbasaur should be filtered out by the search");
  }

  await page.getByRole("button", { name: /charmander/i }).click();
  await page.getByRole("heading", { name: "Charmander" }).waitFor({ timeout: 15000 });
  await page.getByText("Fire", { exact: true }).waitFor();

  console.log("Pokedex smoke passed: list, search focus, filter, detail panel");
} finally {
  await browser.close();
}
