
export interface OpeArg {
    operation: () => Promise<any>;
    successPortFunc: any;
    errorPortFunc?: any;
    successResponseConverter?: (any) => any;
    errorConverter?: (any) => any;
}

export async function ope( arg: OpeArg ) {
    try {
        const res = await arg.operation();
        const resC = arg.successResponseConverter ? arg.successResponseConverter(res) : res;
        arg.successPortFunc.send(resC);
    } catch (err) {
        if (!arg.errorPortFunc) return;
        const res = arg.errorConverter ? arg.errorConverter(err) : err.code
        arg.errorPortFunc.send(res);
    }
}