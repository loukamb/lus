import { defineCollection, z } from "astro:content"
import { glob } from "astro/loaders"

const news = defineCollection({
  schema: z.object({
    title: z.string(),
    date: z.string(),
  }),
  loader: glob({ base: "./src/news/", pattern: "*.md" }),
})

export const collections = { news }
