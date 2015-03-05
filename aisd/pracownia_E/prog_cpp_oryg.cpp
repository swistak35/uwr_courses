#include <algorithm>
#include <cstdio>
#include <cstdlib>
#include <queue>
#include <list>
#include <functional>

using namespace std;

// można przemnożyć wszystko przez 2 i dzialac tylko na intach?

typedef struct segment {
    int x1, y1, x2, y2;
} Segment;

typedef struct event {
    char t;
    float x,y;
    Segment * seg;
} Event;

// p1 > p2      ->      res > 0
// p1 = p2      ->      res = 0
// p1 < p2      ->      res < 0
int point_cmp(int x1, int y1, int x2, int y2) {
    if (x1 != x2) {
        return (x1-x2);
    } else {
        return (y1-y2);
    }
}

// czy e1 < e2 ?
bool event_cmp(Event * e1, Event * e2) {
    if (e1->x < e2->x || (e1->x == e2->x && e1->y < e2->y)) {
        return true;
    } else {
        return false;
    }
}

bool is_between(Segment * seg, int x, int y) {
    int cpr = (y - seg->y1) * (seg->x2 - seg->x1) - (x - seg->x1) * (seg->y2 - seg->y1);
    if (cpr != 0) {
        return false;
    }

    int dpr = (x - seg->x1) * (seg->x2 - seg->x1) + (y - seg->y1)*(seg->y2 - seg->y1);
    if (dpr < 0) {
        return false;
    }

    int sl = (seg->x2 - seg->x1)*(seg->x2 - seg->x1) + (seg->y2 - seg->y1)*(seg->y2 - seg->y1);
    if (dpr > sl) {
        return false;
    }

    return true;
}

int main() {
    int n;
    scanf("%d", &n);
    Segment * segs[n];
    list<Segment*> current;
    priority_queue<Event*, vector<Event*>, function<bool(Event*, Event*)>> eventq(event_cmp);
    // sprawdzic czy dobrze dziala dla odcinkow rownoleglych do sweep line

    for (int i = 0; i < n; i++) {
        int x1, y1, x2, y2;
        Segment * new_segment;

        scanf("%d %d %d %d", &x1, &y1, &x2, &y2);
        new_segment = (Segment *) malloc(sizeof(Segment));
        segs[i] = new_segment;

        if (point_cmp(x1,y1,x2,y2) <= 0) {
            new_segment->x1 = x1;
            new_segment->y1 = y1;
            new_segment->x2 = x2;
            new_segment->y2 = y2;
        } else {
            new_segment->x1 = x2;
            new_segment->y1 = y2;
            new_segment->x2 = x1;
            new_segment->y2 = y1;
        }

        Event * new_event;
        new_event = (Event *) malloc(sizeof(Event));
        new_event->x = new_segment->x1;
        new_event->y = new_segment->y1;
        new_event->seg = new_segment;
        new_event->t = 't';
        eventq.push(new_event);

        new_event = (Event *) malloc(sizeof(Event));
        new_event->x = new_segment->x2;
        new_event->y = new_segment->y2;
        new_event->seg = new_segment;
        new_event->t = 'b';
        eventq.push(new_event);
    }

    float point_x, point_y;

    while (!eventq.empty()) {

    }


    return 0;
}
