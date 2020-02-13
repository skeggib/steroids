import { iso_today } from '../../support/helpers.js'

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
