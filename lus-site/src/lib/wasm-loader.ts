/**
 * Client-side WASM loader for Lus playground
 * Dynamically fetches WASM builds from GitHub releases
 */

const GITHUB_REPO = "loukamb/lus"
const WASM_ASSETS = { js: "lus.js", wasm: "lus.wasm" }

export interface WasmRelease {
  version: string
  jsUrl: string
  wasmUrl: string
  isStable: boolean
}

export interface WasmReleases {
  stable: WasmRelease | null
  unstable: WasmRelease | null
}

interface GitHubAsset {
  name: string
  browser_download_url: string
}

interface GitHubRelease {
  tag_name: string
  prerelease: boolean
  assets: GitHubAsset[]
}

/**
 * Fetches available WASM releases from GitHub
 */
export async function getWasmReleases(): Promise<WasmReleases> {
  const res = await fetch(
    `https://api.github.com/repos/${GITHUB_REPO}/releases`,
    {
      headers: {
        Accept: "application/vnd.github.v3+json",
      },
    }
  )

  if (!res.ok) {
    console.error(`GitHub API error: ${res.status}`)
    return { stable: null, unstable: null }
  }

  const releases: GitHubRelease[] = await res.json()

  const findRelease = (prerelease: boolean): WasmRelease | null => {
    const rel = releases.find((r) => r.prerelease === prerelease)
    if (!rel) return null

    const assets = new Map(
      rel.assets.map((a) => [a.name, a.browser_download_url])
    )
    const jsUrl = assets.get(WASM_ASSETS.js)
    const wasmUrl = assets.get(WASM_ASSETS.wasm)

    if (!jsUrl || !wasmUrl) return null

    return {
      version: rel.tag_name,
      jsUrl,
      wasmUrl,
      isStable: !prerelease,
    }
  }

  return {
    stable: findRelease(false),
    unstable: findRelease(true),
  }
}

/**
 * Lus WASM module interface
 */
export interface LusModule {
  execute: (code: string) => string
  destroy: () => void
}

/**
 * Loads and initializes the Lus WASM module from a release
 */
export async function loadLusWasm(release: WasmRelease): Promise<LusModule> {
  // Fetch the JS loader
  const jsResponse = await fetch(release.jsUrl)
  const jsCode = await jsResponse.text()

  // Create a blob URL to import the module
  const blob = new Blob([jsCode], { type: "application/javascript" })
  const blobUrl = URL.createObjectURL(blob)

  try {
    // Dynamically import the module
    const moduleFactory = (await import(/* @vite-ignore */ blobUrl)).default

    // Initialize with locateFile to find the WASM
    const Module = await moduleFactory({
      locateFile: (path: string) => {
        if (path.endsWith(".wasm")) {
          return release.wasmUrl
        }
        return path
      },
    })

    // Wrap the C functions
    const lus_create = Module.cwrap("lus_create", "number", [])
    const lus_execute = Module.cwrap("lus_execute", "string", [
      "number",
      "string",
    ])
    const lus_destroy = Module.cwrap("lus_destroy", null, ["number"])

    // Create a state
    const state = lus_create()
    if (!state) {
      throw new Error("Failed to create Lus state")
    }

    return {
      execute: (code: string) => lus_execute(state, code),
      destroy: () => {
        lus_destroy(state)
        URL.revokeObjectURL(blobUrl)
      },
    }
  } catch (e) {
    URL.revokeObjectURL(blobUrl)
    throw e
  }
}

