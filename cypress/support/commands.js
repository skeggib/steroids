// ***********************************************
// This example commands.js shows you how to
// create various custom commands and overwrite
// existing commands.
//
// For more comprehensive examples of custom
// commands please read more here:
// https://on.cypress.io/custom-commands
// ***********************************************
//
//
// -- This is a parent command --
// Cypress.Commands.add("login", (email, password) => { ... })
//
//
// -- This is a child command --
// Cypress.Commands.add("drag", { prevSubject: 'element'}, (subject, options) => { ... })
//
//
// -- This is a dual command --
// Cypress.Commands.add("dismiss", { prevSubject: 'optional'}, (subject, options) => { ... })
//
//
// -- This will overwrite an existing command --
// Cypress.Commands.overwrite("visit", (originalFn, url, options) => { ... })

/**
 * Navigate to the exercise creation page using buttons.
 */
Cypress.Commands.add('navigate_to_create_exercise_page', () => {
    cy.contains('Create an exercise').click()
})

/**
 * Fill the exercise creation input fields.
 * @param {string} exercise_name The exercise name
 * @param {integer} sets_number The numbers of sets
 * @param {integer} repetitions_number The number of repetitions
 * @param {string} date The date in ISO format
 */
Cypress.Commands.add('fill_input_fields', (exercise_name, sets_number, repetitions_number, date) => {
    cy.contains('Name').next('input').type(exercise_name)
    cy.contains('Sets number').next('input').type(sets_number)
    cy.contains('Repetitions number').next('input').type(repetitions_number)
    cy.contains('Date').next('input').type(date)
})

/**
 * Click the create button in the exercice creation page.
 */
Cypress.Commands.add('click_create_button', () => {
    cy.contains('button', 'Create').click()
})

Cypress.Commands.add('create_exercise', (exercise_name, sets_number, repetitions_number, date) => {
    cy.navigate_to_create_exercise_page()
    cy.fill_input_fields(exercise_name, sets_number, repetitions_number, date)
    cy.click_create_button()
})

Cypress.Commands.add('long_press', { prevSubject: true }, (subject, duration = 1000) => {
    cy.wrap(subject).trigger('mousedown',
        {
            altKey: false,
            button: 2,
            ctrlKey: false,
            offsetX: 0,
            offsetY: 0,
            shiftKey: false,
        })
    cy.wait(duration)
    cy.wrap(subject).trigger('mouseup',
        {
            altKey: false,
            button: 2,
            ctrlKey: false,
            offsetX: 0,
            offsetY: 0,
            shiftKey: false,
        })
})
