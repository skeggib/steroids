const today = new Date()
const zeroPad = (num, places) => String(num).padStart(places, '0')

export const iso_today =
    zeroPad(today.getFullYear(), 4) +
    '-' +
    zeroPad(today.getMonth() + 1, 2) +
    '-' +
    zeroPad(today.getDate(), 2)

export const iso_tomorrow =
    zeroPad(today.getFullYear(), 4) +
    '-' +
    zeroPad(today.getMonth() + 1, 2) +
    '-' +
    zeroPad(today.getDate() + 1, 2)

export function generateUUID() { // Public Domain/MIT
    var d = new Date().getTime();//Timestamp
    var d2 = (performance && performance.now && (performance.now() * 1000)) || 0;//Time in microseconds since page-load or 0 if unsupported
    return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function (c) {
        var r = Math.random() * 16;//random number between 0 and 16
        if (d > 0) {//Use timestamp until depleted
            r = (d + r) % 16 | 0;
            d = Math.floor(d / 16);
        } else {//Use microseconds since page-load if supported
            r = (d2 + r) % 16 | 0;
            d2 = Math.floor(d2 / 16);
        }
        return (c === 'x' ? r : (r & 0x3 | 0x8)).toString(16);
    });
}
