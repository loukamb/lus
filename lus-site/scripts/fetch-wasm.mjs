/**
 * Fetches WASM files from GitHub releases at build time
 * Run this before `astro build` to include WASM in static output
 */

import { mkdir, writeFile } from "fs/promises"
import { existsSync } from "fs"
import { join, dirname } from "path"
import { fileURLToPath } from "url"

const __dirname = dirname(fileURLToPath(import.meta.url))
const GITHUB_REPO = "loukamb/lus"
const OUTPUT_DIR = join(__dirname, "..", "public", "wasm")
const MANIFEST_PATH = join(OUTPUT_DIR, "manifest.json")

async function fetchReleases() {
  const res = await fetch(
    `https://api.github.com/repos/${GITHUB_REPO}/releases`,
    {
      headers: { Accept: "application/vnd.github.v3+json" },
    }
  )

  if (!res.ok) {
    throw new Error(`GitHub API error: ${res.status}`)
  }

  return res.json()
}

async function downloadFile(url, dest) {
  console.log(`  Downloading ${url}`)
  const res = await fetch(url, {
    headers: { "User-Agent": "lus-site-build" },
  })

  if (!res.ok) {
    throw new Error(`Failed to download ${url}: ${res.status}`)
  }

  const buffer = await res.arrayBuffer()
  await mkdir(dirname(dest), { recursive: true })
  await writeFile(dest, Buffer.from(buffer))
}

async function main() {
  console.log("Fetching releases from GitHub...")
  const releases = await fetchReleases()

  // Sort by date descending
  const sorted = releases.sort(
    (a, b) => new Date(b.published_at).getTime() - new Date(a.published_at).getTime()
  )

  const manifest = { stable: null, unstable: null }

  for (const isStable of [true, false]) {
    const release = sorted.find((r) => r.prerelease === !isStable)
    if (!release) continue

    const assets = new Map(release.assets.map((a) => [a.name, a.browser_download_url]))
    const jsUrl = assets.get("lus.js")
    const wasmUrl = assets.get("lus.wasm")

    if (!jsUrl || !wasmUrl) {
      console.log(`No WASM assets in ${isStable ? "stable" : "unstable"} release ${release.tag_name}`)
      continue
    }

    const version = release.tag_name
    const versionDir = join(OUTPUT_DIR, version)
    const type = isStable ? "stable" : "unstable"

    console.log(`\nDownloading ${type} release: ${version}`)

    await downloadFile(jsUrl, join(versionDir, "lus.js"))
    await downloadFile(wasmUrl, join(versionDir, "lus.wasm"))

    manifest[type] = { version }
  }

  // Write manifest for the client to know available versions
  await writeFile(MANIFEST_PATH, JSON.stringify(manifest, null, 2))
  console.log(`\nWrote manifest to ${MANIFEST_PATH}`)
  console.log("Done!")
}

main().catch((err) => {
  console.error(err)
  process.exit(1)
})

