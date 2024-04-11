import child_process from "child_process";

export async function command(cmdline: string): Promise<{}> {
    return new Promise<{}>((resolve, reject) =>
        child_process.exec(cmdline + " 2>&1", (err, stdout) => {
            if (err) {
                reject(err);
                return;
            }
            console.log(stdout);
            resolve({});
        })
    );
}
