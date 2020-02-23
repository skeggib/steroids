import { iso_today } from '../../support/helpers.js'

describe('Loading directly', function () {
    context('the show day page', function () {
        it('shows the page', function () {
            cy.add_exercise_to_storage('Exercise name 1', 10, 20, iso_today)
            cy.visit('http://localhost:8000/')
            cy.visit('http://localhost:8000/day/' + iso_today)
            cy.contains('Exercise name 1')
        })
    })

    context('the create exercise page', function () {
        it('shows the page', function () {
            cy.visit('http://localhost:8000/exercises/create')
            cy.contains('Name').next('input')
            cy.contains('Sets number').next('input')
            cy.contains('Repetitions number').next('input')
            cy.contains('Date').next('input')
        })
    })

    // TODO: last exercises page
    // TODO: edit exercise page
    // TODO: delete exercise page
})
