import { client } from "@/client/client.gen"

export function initializeApiClient() {
  client.setConfig({
    baseUrl: import.meta.env.VITE_API_URL || "",
  })
}
