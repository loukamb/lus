// @ts-check
import { defineConfig } from "astro/config"

import tailwindcss from "@tailwindcss/vite"

import mdx from "@astrojs/mdx"

import sitemap from "@astrojs/sitemap";

// https://astro.build/config
export default defineConfig({
  output: "static",
  site: "https://lus.dev",
  vite: {
    plugins: [tailwindcss()],
  },
  markdown: {
    shikiConfig: {
      themes: {
        light: "github-light",
        dark: "github-light",
      },
    },
  },
  integrations: [mdx(), sitemap()],
})