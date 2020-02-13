const today = new Date()
const zeroPad = (num, places) => String(num).padStart(places, '0')
const iso_today =
    zeroPad(today.getFullYear(), 4) +
    '-' +
    zeroPad(today.getMonth() + 1, 2) +
    '-' +
    zeroPad(today.getDate(), 2)
const iso_tomorrow =
    zeroPad(today.getFullYear(), 4) +
    '-' +
    zeroPad(today.getMonth() + 1, 2) +
    '-' +
    zeroPad(today.getDate() + 1, 2)

describe('A long press', function () {
    it('displays the action bar', function () {
        cy.visit('http://localhost:8000/')
        cy.create_exercise('Exercise name', 10, 20, iso_today)
        cy.get('.dayLink').click()
        cy.get('.action-bar').should('not.be.visible')
        cy.get('.exercise').long_press()
        cy.get('.action-bar').should('be.visible')
    })
})

describe('The action bar', function () {

    this.beforeEach(function () {
        cy.visit('http://localhost:8000/')
        cy.create_exercise('Exercise name', 10, 20, iso_today)
        cy.get('.dayLink').click()
        cy.get('.exercise').long_press()
    })

    it('contains a delete button', function () {
        cy.get('.action-bar').contains('delete')
    })
})
