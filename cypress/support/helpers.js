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
