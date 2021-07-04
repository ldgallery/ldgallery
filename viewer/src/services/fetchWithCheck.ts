export default class FetchWithCheck {
  static async get(url: RequestInfo): Promise<Response> {
    const response = await fetch(url);
    if (!response.ok) throw new Error(`${response.status}: ${response.statusText}`);
    return response;
  }
}
