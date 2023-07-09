
export interface KayoinobaAttributeIconUrls {
    taisou: string;
    noutore: string;
    ongaku: string;
    insyokuari: string;
    undou: string;
    free: string;
}

export const urls: KayoinobaAttributeIconUrls = {
    taisou: new URL("../img/kayoinoba-attribute-taisou.png?as=webp", import.meta.url).href,
    noutore: new URL("../img/kayoinoba-attribute-noutore.png?as=webp", import.meta.url).href,
    ongaku: new URL("../img/kayoinoba-attribute-ongaku.png?as=webp", import.meta.url).href,
    insyokuari: new URL("../img/kayoinoba-attribute-insyokuari.png?as=webp", import.meta.url).href,
    undou: new URL("../img/kayoinoba-attribute-undou.png?as=webp", import.meta.url).href,
    free: new URL("../img/kayoinoba-attribute-free.png?as=webp", import.meta.url).href,
}
// 下記のように動的にURLを生成しようとしたが、そうするとparcelによるURL変換が効かなくなるため断念.
// ["taisou", "noutore", "ongaku", "insyokuari", "undou", "free"].reduce((acc, name) => ...