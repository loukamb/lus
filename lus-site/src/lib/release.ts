const GITHUB_REPO = "loukamb/lus"

export interface ReleaseLinks {
  windows?: string
  macos?: string
  linuxGlibc?: string
  linuxMusl?: string
  wasmJs?: string
  wasmBinary?: string
  sourceZip?: string
  sourceTarball?: string
}

export interface ReleaseInfo {
  version: string
  releaseDate: string
  links: ReleaseLinks
  htmlUrl: string
}

interface GitHubAsset {
  name: string
  browser_download_url: string
}

interface GitHubRelease {
  tag_name: string
  published_at: string
  html_url: string
  prerelease: boolean
  assets: GitHubAsset[]
  zipball_url: string
  tarball_url: string
}

const ASSET_NAMES = {
  windows: "lus-windows.exe",
  macos: "lus-macos",
  linuxGlibc: "lus-linux",
  linuxMusl: "lus-linux-musl",
  wasmJs: "lus.js",
  wasmBinary: "lus.wasm",
}

function parseAssetLinks(release: GitHubRelease): ReleaseLinks {
  const assetMap = new Map(
    release.assets.map((a) => [a.name, a.browser_download_url])
  )

  return {
    windows: assetMap.get(ASSET_NAMES.windows),
    macos: assetMap.get(ASSET_NAMES.macos),
    linuxGlibc: assetMap.get(ASSET_NAMES.linuxGlibc),
    linuxMusl: assetMap.get(ASSET_NAMES.linuxMusl),
    wasmJs: assetMap.get(ASSET_NAMES.wasmJs),
    wasmBinary: assetMap.get(ASSET_NAMES.wasmBinary),
    sourceZip: release.zipball_url,
    sourceTarball: release.tarball_url,
  }
}

function formatReleaseDate(isoDate: string): string {
  return isoDate.split("T")[0]
}

async function fetchGitHubReleases(): Promise<GitHubRelease[] | null> {
  try {
    const response = await fetch(
      `https://api.github.com/repos/${GITHUB_REPO}/releases`,
      {
        headers: {
          Accept: "application/vnd.github.v3+json",
        },
      }
    )

    if (!response.ok) {
      console.error(`GitHub API error: ${response.status}`)
      return null
    }

    return response.json()
  } catch (error) {
    console.error("Failed to fetch GitHub releases:", error)
    return null
  }
}

export async function getLatestStableRelease(): Promise<ReleaseInfo | null> {
  const releases = await fetchGitHubReleases()
  if (!releases) return null

  const stable = releases.find((r) => !r.prerelease)
  if (!stable) return null

  return {
    version: stable.tag_name,
    releaseDate: formatReleaseDate(stable.published_at),
    links: parseAssetLinks(stable),
    htmlUrl: stable.html_url,
  }
}

export async function getLatestUnstableRelease(): Promise<ReleaseInfo | null> {
  const releases = await fetchGitHubReleases()
  if (!releases) return null

  const unstable = releases.find((r) => r.prerelease)
  if (!unstable) return null

  return {
    version: unstable.tag_name,
    releaseDate: formatReleaseDate(unstable.published_at),
    links: parseAssetLinks(unstable),
    htmlUrl: unstable.html_url,
  }
}

export async function getAllReleases(): Promise<{
  stable: ReleaseInfo | null
  unstable: ReleaseInfo | null
}> {
  const releases = await fetchGitHubReleases()

  if (!releases) {
    return { stable: null, unstable: null }
  }

  // Sort by published_at descending to ensure we get the latest releases
  const sorted = [...releases].sort(
    (a, b) =>
      new Date(b.published_at).getTime() - new Date(a.published_at).getTime()
  )

  const stableRelease = sorted.find((r) => !r.prerelease)
  const unstableRelease = sorted.find((r) => r.prerelease)

  return {
    stable: stableRelease
      ? {
          version: stableRelease.tag_name,
          releaseDate: formatReleaseDate(stableRelease.published_at),
          links: parseAssetLinks(stableRelease),
          htmlUrl: stableRelease.html_url,
        }
      : null,
    unstable: unstableRelease
      ? {
          version: unstableRelease.tag_name,
          releaseDate: formatReleaseDate(unstableRelease.published_at),
          links: parseAssetLinks(unstableRelease),
          htmlUrl: unstableRelease.html_url,
        }
      : null,
  }
}
