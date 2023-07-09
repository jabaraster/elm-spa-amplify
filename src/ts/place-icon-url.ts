
export interface PlaceIconUrls {
    category1: string;
    category2: string;
    category3: string;
}

export const urls : PlaceIconUrls = {
    category1: new URL("../img/place-icon-CATEGORY1.jpeg?as=webp&width=32&height=32", import.meta.url).href,
    category2: new URL("../img/place-icon-CATEGORY2.jpeg?as=webp&width=32&height=32", import.meta.url).href,
    category3: new URL("../img/place-icon-CATEGORY3.jpeg?as=webp&width=32&height=32", import.meta.url).href,
}