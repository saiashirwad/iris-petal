import { chromium } from "playwright";

const baseUrl = process.env.POKEDEX_URL ?? "http://localhost:5173/";
const browser = await chromium.launch();

try {
  const page = await browser.newPage({ viewport: { width: 1280, height: 900 } });
  await page.goto(baseUrl, { waitUntil: "domcontentloaded" });

  await page.getByRole("button", { name: /bulbasaur/i }).waitFor({ timeout: 15000 });

  const detailPanel = page.locator(".detail-panel");
  const beforeHeight = (await detailPanel.boundingBox())?.height ?? 0;
  const beforeScroll = await page.evaluate(() => window.scrollY);

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

  await page.route("**/pokeapi.co/api/v2/pokemon/charmander", async (route) => {
    await new Promise((resolve) => setTimeout(resolve, 700));
    await route.continue();
  });

  await page.getByRole("button", { name: /charmander/i }).click();
  await page.locator(".detail-panel .skeleton-card").waitFor({ timeout: 5000 });
  const loadingHeight = (await detailPanel.boundingBox())?.height ?? 0;

  await page.getByRole("heading", { name: "Charmander" }).waitFor({ timeout: 15000 });
  await page.getByText("Fire", { exact: true }).waitFor();
  const afterHeight = (await detailPanel.boundingBox())?.height ?? 0;
  const afterScroll = await page.evaluate(() => window.scrollY);

  if (Math.abs(beforeHeight - loadingHeight) > 1 || Math.abs(loadingHeight - afterHeight) > 1) {
    throw new Error(
      `detail panel shifted: before=${beforeHeight} loading=${loadingHeight} after=${afterHeight}`,
    );
  }
  if (beforeScroll !== afterScroll) {
    throw new Error(`page scrolled during detail load: ${beforeScroll} -> ${afterScroll}`);
  }

  await search.fill("");
  await page.getByRole("button", { name: /bulbasaur/i }).waitFor();

  for (let attempt = 0; attempt < 12; attempt += 1) {
    await page.evaluate(() => window.scrollTo(0, document.body.scrollHeight));
    await page.waitForTimeout(500);
    if ((await page.getByText("All 151 Pokémon loaded.").count()) > 0) break;
  }
  await page.getByText("All 151 Pokémon loaded.").waitFor({ timeout: 15000 });

  console.log(
    `Pokedex smoke passed: list, focus, filter, detail, pagination, stable detail panel (${afterHeight}px)`,
  );
} finally {
  await browser.close();
}
