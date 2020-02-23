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

function generateUUID() { // Public Domain/MIT
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

Cypress.Commands.add('add_exercise_to_storage', (exercise_name, sets_number, repetitions_number, date) => {
    var storage = JSON.parse(localStorage.getItem('storage'))
    if (storage === null) {
        storage = {
            "version": 2,
            "exercises": []
        }
    }
    storage.exercises.push({
        "id": generateUUID(),
        "name": exercise_name,
        "setsNumber": sets_number,
        "repetitionsNumber": repetitions_number,
        "date": date,
        "validated": false
    })
    localStorage.setItem('storage', JSON.stringify(storage))
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
