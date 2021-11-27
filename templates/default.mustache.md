[{{calendarTitle}}]({{calendarUrl}})

{{#calendarEntries}}
* [{{entryTitle}}]({{entryUrl}})
  - {{entryAuthor}}
  {{#entrySummary}}
  - {{.}}
  {{/entrySummary}}
{{/calendarEntries}}
{{^calendarEntries}}
No Entries
{{/calendarEntries}}
