import Session from '../model/Session';
import Exercice from '../model/Exercice';
import Series from '../model/Series';
import { Observable } from 'rxjs';
import { EventEmitter } from '@angular/core';

export default interface IStorage {

    sessionsChanged: EventEmitter<Session[]>;
    exerciceTemplatesChanged: EventEmitter<Exercice[]>;

    getAllSessions(): Observable<Session[]>;
    getSession(id: string): Observable<Session>;
    createSession(name: string): Observable<Session>;

    getAllExerciceTemplates(): Observable<Exercice[]>;
    getExercice(id: string): Observable<Exercice>;
    addExercice(sessionId: string, name: string): Observable<Exercice>;

    addSeries(exerciceId: string, repetition: number, weight: number, rating: number): Observable<Series>;
}
