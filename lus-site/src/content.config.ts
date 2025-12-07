import { defineCollection, z } from "astro:content"
import { glob } from "astro/loaders"

const news = defineCollection({
  schema: z.object({
    title: z.string(),
    date: z.string(),
  }),
  loader: glob({ base: "./src/news/", pattern: "*.md" }),
})

const manual = defineCollection({
  schema: z.object({
    title: z.string(),
    order: z.number(),
    acquis: z.boolean().optional(),
    draft: z.boolean().optional(),
  }),
  loader: glob({ base: "./src/manual/", pattern: "*.mdx" }),
})

export const collections = { news, manual }
