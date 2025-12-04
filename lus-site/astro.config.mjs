// @ts-check
import { defineConfig } from "astro/config"

import tailwindcss from "@tailwindcss/vite"

import mdx from "@astrojs/mdx"

// https://astro.build/config
export default defineConfig({
  output: "static",

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

  integrations: [mdx()],
})
