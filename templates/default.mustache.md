[{{calendarTitle}}]({{calendarUrl}})

{{#calendarEntries}}
* [{{entryTitle}}]({{entryUrl}})
  - {{entryAuthor}}
  {{#entryPublished}}
  - {{month}} / {{day}}
  {{/entryPublished}}
  {{#entrySummary}}
  > {{.}}
  {{/entrySummary}}
{{/calendarEntries}}
{{^calendarEntries}}
No Entries
{{/calendarEntries}}
