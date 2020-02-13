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

describe('The action bar', function () {

    this.beforeEach(function () {
        cy.visit('http://localhost:8000/')
        cy.create_exercise('Exercise name', 10, 20, iso_today)
        cy.get('.dayLink').click()
        cy.get('.exercise').long_press()
    })

    it('allows to delete an exercise', function () {
        cy.get('.action-bar').contains('delete').click({ force: true })
        cy.location().should((location) => {
            expect(location.pathname).to.eq('/')
        })
        cy.get('.dayLink').should('have.length', 0)
    })
})
