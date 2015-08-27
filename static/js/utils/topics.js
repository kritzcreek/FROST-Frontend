export function topicTypeColor(topicType) {
  switch (topicType) {
  case 'Discussion':
    return 'topic-discussion';
  case 'Presentation':
    return 'topic-presentation';
  case 'Workshop':
    return 'topic-workshop';
  default:
    return 'topic-default';
  }
}
