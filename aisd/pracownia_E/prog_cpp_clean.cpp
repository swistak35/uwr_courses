#include <algorithm>
#include <cstdio>
#include <cstdlib>
#include <queue>
#include <list>
#include <functional>
#include <set>

#define DEBUG true

using namespace std;

typedef struct point {
    int x;
    int y;
} Point;

typedef struct segment {
    char dir;
    Point * left;
    Point * right;
} Segment;

typedef struct event {
    char t;
    Point * p;
    Segment * seg;
} Event;

void print_point(Point * p) {
    printf("(%d,%d)", p->x, p->y);
}

void print_seg(Segment * s) {
    printf("[");
    print_point(s->left);
    printf(" | ");
    print_point(s->right);
    printf("]");
}

void print_event(Event * e) {
    printf("Event(%c,", e->t);
    print_point(e->p);
    if (e->seg != NULL) {
        printf(",");
        print_seg(e->seg);
    }
    printf(")");
}

void print_list(list<Segment*> lst) {
    for (list<Segment*>::iterator it = lst.begin(); it != lst.end(); ++it) {
        printf("\n\t");
        print_seg(*it);
    }
    printf("\nKoniec listy.\n");
}

void set_dir(Segment *seg) {
    if (seg->left->x == seg->right->x) { // pionowa
        seg->dir = 3;
    } else if (seg->left->y == seg->right->y) { // pozioma
        seg->dir = 1;
    } else if (seg->left->y > seg->right-> y) { // malejaca
        seg->dir = 0;
    } else { // rosnaca
        seg->dir = 2;
    }
}

// p1 > p2      ->      res > 0
// p1 = p2      ->      res = 0
// p1 < p2      ->      res < 0
int point_cmp(Point * p1, Point * p2) {
    if (p1->x != p2->x) {
        return (p1->x - p2->x);
    } else {
        return (p1->y - p2->y);
    }
}

// czy e1 < e2 ?
bool event_cmp(Event * e1, Event * e2) {
    return (point_cmp(e1->p, e2->p) > 0);
}

bool is_between(Segment * seg, Point * p) {
    Point * p1 = seg->left;
    Point * p2 = seg->right;

    int cpr = (p->y - p1->y) * (p2->x - p1->x) - (p->x - p1->x) * (p2->y - p1->y);
    if (cpr != 0) {
        return false;
    }

    int dpr = (p->x - p1->x) * (p2->x - p1->x) + (p->y - p1->y)*(p2->y - p1->y);
    if (dpr < 0) {
        return false;
    }

    int sl = (p2->x - p1->x)*(p2->x - p1->x) + (p2->y - p1->y)*(p2->y - p1->y);
    if (dpr > sl) {
        return false;
    }

    return true;
}

void tab_clear(Segment * tab[]) {
    for (int i = 0; i < 4; i++) {
        tab[i] = NULL;
    }
}

int count_not_nulls(Segment * tab[]) {
    int ret = 0;
    for (int i = 0; i < 4; i++) {
        if (tab[i] != NULL) {
            ret++;
        }
    }
    return ret;
}

int IsPointInBoundingBox(int x1, int y1, int x2, int y2, int px, int py) {
    int left, top, right, bottom;
    
    if(x1 < x2) {
        left = x1;
        right = x2;
    } else {
        left = x2;
        right = x1;
    }
    if(y1 < y2)
    {
        top = y2;
        bottom = y1;
    }
    else
    {
        top = y1;
        bottom = y2;
    }
//    printf("left: %f | right: %f | top: %f | bottom: ")
    if( px >= left && px <= right &&
            py >= bottom && py <= top )
    {
        return 1;
    }
    else {
        return 0;
    }
}

int check_cross_with_line(Segment * s, int x) {
    int slope, b;
    if (s->right->x != s->left->x) {
        if (s->dir == 0) {
            slope = -1;
        } else if (s->dir == 1) {
            slope = 0;
        } else if (s->dir == 2) {
            slope = 1;
        }
//        if (s->right->y != s->left->y) {
//            slope = float(s->right->x - s->left->x) / (s->right->y - s->left->y);
//        } else {
//            slope = 0;
//        }
        b = s->right->y - slope * s->right-> x;

        if (DEBUG) { printf("slope: %f | b: %f | res: %f\n", slope, b, slope*x + b); }
        return (slope*x + b);
    } else {
        return (max(s->left->y, s->right->y)); // to się da konkretnie wskazać.
    }
}

Point * check_segment_cross(Segment * s1, Segment * s2) {
    float l1x1 = s1->left->x;
    float l1y1 = s1->left->y;
    float l1x2 = s1->right->x;
    float l1y2 = s1->right->y;
    float l2x1 = s2->left->x;
    float l2y1 = s2->left->y;
    float l2x2 = s2->right->x;
    float l2y2 = s2->right->y;
    float m1, c1, m2, c2;
    float intersection_X, intersection_Y;
    float dx, dy;

    if (s1->dir != 3) {
        dx = l1x2 - l1x1;
        dy = l1y2 - l1y1;
        m1 = dy / dx;
        c1 = l1y1 - m1 * l1x1; // which is same as y2 - slope * x2
    }

    if (s2->dir != 3) {
        dx = l2x2 - l2x1;
        dy = l2y2 - l2y1;

        m2 = dy / dx;
        c2 = l2y1 - m2 * l2x1; // which is same as y2 - slope * x2
    }

    if (s1->dir != 3 && s2->dir != 3) {
        if( (m1 - m2) == 0)
            return NULL;
        else {
            intersection_X = (c2 - c1) / (m1 - m2);
            intersection_Y = m1 * intersection_X + c1;
        }
    } else if (s1->dir != 3) {
	    intersection_X = s2->left->x;
            intersection_Y = check_cross_with_line(s1, s2->left->x);
    } else if (s2->dir != 3) {
	    intersection_X = s1->left->x;
	    intersection_Y = check_cross_with_line(s2, s1->left->x);
    } else {
	    if (point_cmp(s1->left, s2->right) == 0) {
		    intersection_X = s1->left->x;
		    intersection_Y = s1->left->y;
	} else {
		intersection_X = s1->right->x;
		intersection_Y = s1->right->y;
	}
    }


    if (DEBUG) {
        printf("Obliczony punkt przeciecia: (%f, %f)\n", intersection_X, intersection_Y);

        printf("cond 1: %d | cond 2: %d\n", IsPointInBoundingBox(l1x1, l1y1, l1x2, l1y2, intersection_X, intersection_Y), IsPointInBoundingBox(l2x1, l2y1, l2x2, l2y2, intersection_X, intersection_Y));
        printf("l2x1: %f | l2y1: %f | l2x2: %f | l2y2: %f\n", l2x1, l2y1, l2x2, l2y2);

        printf("Sprawdzamy czy segmenty ");
        print_seg(s1);
        printf(" i ");
        print_seg(s2);
        printf(" przecinaja sie: ");
    }
    if(IsPointInBoundingBox(l1x1, l1y1, l1x2, l1y2, intersection_X, intersection_Y) == 1 &&
        IsPointInBoundingBox(l2x1, l2y1, l2x2, l2y2, intersection_X, intersection_Y) == 1)
    {
        Point * p;
        p = (Point *) malloc(sizeof(Point));
        p->x = intersection_X;
        p->y = intersection_Y;
        if (DEBUG) { print_point(p); printf(" TAK\n"); }
        return p;
    }
    else {
        if (DEBUG) printf("NIE\n");
        return NULL;
    }
}



int main() {
    int n;
    int tmp;
    scanf("%d", &n);
    list<Segment*> current;
    priority_queue<Event*, vector<Event*>, function<bool(Event*, Event*)>> eventq(event_cmp);

    for (int i = 0; i < n; i++) {
        Segment *new_segment;
        Point *p1, *p2;

        p1 = (Point *) malloc(sizeof(Point));
        p2 = (Point *) malloc(sizeof(Point));
        scanf("%d %d %d %d", &p1->x, &p1->y, &p2->x, &p2->y);
        p1->x *= 2;
        p1->y *= 2;
        p2->x *= 2;
        p2->y *= 2;
        new_segment = (Segment *) malloc(sizeof(Segment));

        if (point_cmp(p1,p2) <= 0) {
            new_segment->left = p1;
            new_segment->right = p2;
        } else {
            new_segment->left = p2;
            new_segment->right = p1;
        }
        set_dir(new_segment);

        Event * new_event;

        new_event = (Event *) malloc(sizeof(Event));
        new_event->p = new_segment->left;
        new_event->seg = new_segment;
        eventq.push(new_event);

        if (DEBUG) { printf("Wrzucam event: "); print_event(new_event); printf("\n"); }

        new_event = (Event *) malloc(sizeof(Event));
        new_event->p = new_segment->right;
	new_event->seg = NULL;
        eventq.push(new_event);

        if (DEBUG) { printf("Wrzucam event: "); print_event(new_event); printf("\n"); }
    }
    if (DEBUG) { printf("Zaladowalem punkty startowe.\n"); }


    Event * e;
    Point * p;
    Segment* new_segs[4];
    Segment* mid_segs[4];
    Segment* end_segs[4];
    list<Segment*>::iterator it2;
    while (!eventq.empty()) {
	e = NULL;
        tab_clear(new_segs);
        tab_clear(mid_segs);
        tab_clear(end_segs);
        if (DEBUG) { printf("Zaczynam nowa iteracje pętli z punktem (%d, %d).\n", eventq.top()->p->x, eventq.top()->p->y); }
        do {
            if (e != NULL) { free(e); }
            e = eventq.top();
            p = e->p;
            eventq.pop();
            // free na evencie!
            if (DEBUG) {
                printf("-- Wyjmuje event: "); print_event(e); printf("\nPozostalo %d elementow.\n", eventq.size());
                // scanf("%d", &tmp);
            }

            if (e->seg != NULL) {
                new_segs[e->seg->dir] = e->seg;
            }
        } while (!eventq.empty() && (point_cmp(e->p, (eventq.top())->p) == 0));
        //free(e);

        bool it2_inserted = false;
        it2 = current.end();
	bool begin_nice_segments = false;
        for (list<Segment*>::iterator it = current.begin(); it != current.end(); ++it) {
            if (point_cmp((*it)->right, p) == 0) {
                end_segs[(*it)->dir] = *it;
                it = current.erase(it);
                it2 = it;
                it2_inserted = true;
                it--;
		begin_nice_segments = true;
            } else if (is_between(*it, p)) {
                mid_segs[(*it)->dir] = *it;
                it = current.erase(it);
                it2 = it;
                it2_inserted = true;
                it--;
		begin_nice_segments = true;
            } else if (begin_nice_segments) {
		break;
	    }
        }
        if (DEBUG) { printf("jajo0    ? %d\n", it2_inserted); }
        if (DEBUG && it2_inserted) {
            printf("++ znalazłem it2 przy wyjmowaniu i jest teraz ustawiony na: ");
            if (it2 != current.end()) { print_seg(*it2); } else { printf("END"); }
            printf("\n");
        }

        if (!it2_inserted) {
            for (list<Segment*>::iterator it = current.begin(); it != current.end(); ++it) {
                if (DEBUG) {
                    printf("Porownywanie %f i %f\n", (float)p->y, check_cross_with_line(*it, p->x));
                    printf("Porownywany segment: "); print_seg(*it); printf("\n");
                }
                if (((float) p->y) < check_cross_with_line(*it, p->x)) {
                    it2 = it;
                    it2_inserted = true;
                    break;
                }
            }
        }
        if (DEBUG) { printf("wolololo\n"); }
        if (DEBUG && it2_inserted) {
            printf("++ znalazłem it2 i jest teraz ustawiony na: ");
            if (it2 != current.end()) { print_seg(*it2); } else { printf("END"); }
            printf("\n");
        }

        it2_inserted = true;

        if ((count_not_nulls(new_segs) + count_not_nulls(mid_segs) + count_not_nulls(end_segs)) > 1) {
            if (DEBUG) {
                printf("======= WYNIK: %f %f\n", ((float)p->x) / 2, ((float)p->y) / 2);
            } else {
                printf("%.1f %.1f\n", ((float)p->x) / 2, ((float)p->y) / 2);
            }
        }
        
	bool left_inserted = false;
        bool right_inserted = false;
        list<Segment*>::iterator it_right;
        list<Segment*>::iterator it_left;
        list<Segment*>::iterator it3;
        for (int i=0; i < 4; i++) {
            if (new_segs[i] != NULL) {
                it_right = current.insert(it2, new_segs[i]);
                right_inserted = true;

                if (!left_inserted) {
                    it_left = it_right;
                    left_inserted = true;
                }
            } else if (mid_segs[i] != NULL) {
                it_right = current.insert(it2, mid_segs[i]);
                right_inserted = true;

                if (!left_inserted) {
                    it_left = it_right;
                    left_inserted = true;
                }
            }
        }
        if (DEBUG) { printf("Wygląd listy po wrzuceniu:\n"); print_list(current); }

        Point * newp;
        Event * newe;

        if (DEBUG) { printf("Wskaznik it_left: "); if (left_inserted) print_seg(*it_left); else printf("NOPE"); printf("\n"); }
        if (left_inserted && it_left != current.begin()) {
            it3 = it_left;
            it3--;
            if (DEBUG) { printf("Wskaznik it3: "); print_seg(*it3); printf("\n"); }
            newp = check_segment_cross(*it3, *it_left);
            // mozliwe ze tutaj tracimy pamiec, bo jesli okaze sie ze jednak eventu nie dodajemy, to ten punkt sobie siedzi w pamieci
            if (newp != NULL && (point_cmp(p, newp) < 0)) {
                newe = (Event *) malloc(sizeof(Event));
                newe->p = newp;
                newe->seg = NULL;
                eventq.push(newe);
                if (DEBUG) { printf("Wrzucam event: "); print_event(newe); printf("\n"); }
            }
        }
        if (DEBUG) { printf("jajo2\n"); }

        if (DEBUG) { printf("Wskaznik it_right: "); if (right_inserted) print_seg(*it_right); else printf("NOPE"); printf("\n"); }
        if (right_inserted) {
            it3 = it_right;
            it3++;
            if (it3 != current.end()) {
                newp = check_segment_cross(*it3, *it_right);
                if (newp != NULL && (point_cmp(p, newp) < 0)) {
                    newe = (Event *) malloc(sizeof(Event));
                    newe->p = newp;
		    newe->seg = NULL;
                    eventq.push(newe);
                    if (DEBUG) { printf("Wrzucam event: "); print_event(newe); printf("\n"); }
                }
            }
        }
        if (DEBUG) { printf("jajo3\n"); }

        if (DEBUG) { printf("Wskaznik it2: "); if (it2_inserted && it2 != current.end()) print_seg(*it2); else printf("NOPE"); printf("\n"); }
        if (!left_inserted && !right_inserted && it2_inserted && it2 != current.begin() && it2 != current.end()) {
            it3 = it2;
            it3--;
            newp = check_segment_cross(*it2, *it3);
            if (newp != NULL && (point_cmp(p, newp) < 0)) {
                newe = (Event *) malloc(sizeof(Event));
                newe->p = newp;
                newe->seg = NULL;
                eventq.push(newe);
                if (DEBUG) { printf("Wrzucam event: "); print_event(newe); printf("\n"); }
            }
        }
        if (DEBUG) { printf("dupa\n"); }
    }


    return 0;
}
