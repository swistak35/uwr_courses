#include <cstdio>
#include <cstdlib>

using namespace std;

typedef struct avl_node {
    int height;
    long long int x;
    struct avl_node * lt;
    struct avl_node * rt;
} Node;

int compare(long long int x, long long int y);
Node * avl_empty();
bool avl_is_empty(Node * tree);
Node * pmin(Node * tree, Node * best);
Node * pmax(Node * tree, Node * best);
Node * avl_upper(Node * tree, long long int x, Node * best);
Node * avl_lower(Node * tree, long long int x, Node * best);
void rotate_right(Node * tree);
int max1(int x, int y);
void rotate_left(Node * tree);
void avl_insert(Node ** tree, long long int x);
void avl_balance(Node * tree);
void avl_balance_right(Node * tree);
void avl_balance_left(Node * tree);
int get_height(Node * tree);
int avl_delete(Node ** tree_ptr, long long int x);
void status(Node * t);
void status_tab(int amount);
void substatus(Node * tree, int depth);


int main() {
    int n;
    scanf("%d\n", &n);

    Node * avl = avl_empty();

    char op;
    long long int x;
    Node * res;

//    status(avl);
//    avl_insert(&avl, 0);
//    status(avl);
//    avl_insert(&avl, 1);
//    status(avl);
    for (int i = 0; i < n; i++) {
        scanf("%c %lld\n", &op, &x);
        //printf("Operacja: %c, argument: %lld\n", op, x);
        //status(avl);
        switch(op) {
        case 'I':
            avl_insert(&avl, x);
            break;
        case 'D':
            if (avl_delete(&avl, x) > 0) {
                printf("OK\n");
            } else {
                printf("BRAK\n");
            }
            break;
        case 'U':
            res = avl_upper(avl, x, NULL);
            if (res == NULL) {
                printf("BRAK\n");
            } else {
                printf("%lld\n", res->x);
            }
            break;
        case 'L':
            res = avl_lower(avl, x, NULL);
            if (res == NULL) {
                printf("BRAK\n");
            } else {
                printf("%lld\n", res->x);
            }
            break;
        }
    }

    return 0;
}

void status(Node * t) {
  substatus(t, 1);
}

void status_tab(int amount) {
  for(int i=1;i<=amount;i++)
    printf("\t");
}

void substatus(Node * tree, int depth) {
  if (avl_is_empty(tree)) {
    status_tab(depth);
    printf("* Lisc\n");
  } else {
    status_tab(depth);
    printf("* [h: %d] [val: %lld] [lt: %p] [rt: %p] [addr: %p]\n", tree->height, tree->x, tree->lt, tree->rt, tree);
    substatus(tree->lt, depth+1);
    substatus(tree->rt, depth+1);
  }
}



long long int find_max(Node * tree) {
    if (avl_is_empty(tree->rt)) {
        return tree->x;
    } else {
        return find_max(tree->rt);
    }
}

int get_height(Node * tree) {
    return (tree == NULL ? 0 : tree->height);
}


// niezbyt optymalne to usuwanie
int avl_delete(Node ** tree_ptr, long long int x) {
    Node * tree = *tree_ptr;
    int res;

    if (avl_is_empty(tree)) {
        return 0;
    } else {
        long long int mx;

        switch (compare(x,tree->x)) {
        case -1:
            res = avl_delete(&(tree->lt), x);
            avl_balance(tree);
            return res;
            break;
        case 0:
            if (avl_is_empty(tree->lt) && avl_is_empty(tree->rt)) {
                free(*tree_ptr);
                *tree_ptr = NULL;
                return 1;
            } else if (avl_is_empty(tree->lt)) {
                free(*tree_ptr);
                *tree_ptr = tree->rt;
                return 1;
            } else if (avl_is_empty(tree->rt)) {
                free(*tree_ptr);
                *tree_ptr = tree->lt;
                return 1;
            } else {
                mx = find_max(tree->lt);
                avl_delete(&(tree->lt), mx);
                tree->x = mx;
                tree->height = max1(get_height(tree->lt), get_height(tree->rt));
                avl_balance(tree);
                return 1;
            }
            break;
        case 1:
            res = avl_delete(&(tree->rt), x);
            avl_balance(tree);
            return res;
            break;
        }
    }
}

int compare(long long int x, long long int y) {
    if (x < y) {
        return -1;
    } else if (x == y) {
        return 0;
    } else {
        return 1;
    }
}

Node * avl_empty() {
    return NULL;
}

bool avl_is_empty(Node * tree) {
    return (tree == NULL);
}

Node * pmin(Node * tree, Node * best) {
    if (avl_is_empty(best)) {
        return tree;
    } else {
        return (tree->x < best->x ? tree : best);
    }
}

Node * pmax(Node * tree, Node * best) {
    if (avl_is_empty(best)) {
        return tree;
    } else {
        return (tree->x > best->x ? tree : best);
    }
}

Node * avl_upper(Node * tree, long long int x, Node * best) {
    if (avl_is_empty(tree)) {
        return best;
    } else {
        switch(compare(x, tree->x)) {
        case -1:
            return avl_upper(tree->lt, x, pmin(tree, best));
            break;
        case 0:
            return tree;
            break;
        case 1:
            return avl_upper(tree->rt, x, best);
            break;
        }
    }
}

Node * avl_lower(Node * tree, long long int x, Node * best) {
    if (avl_is_empty(tree)) {
        return best;
    } else {
        switch(compare(x, tree->x)) {
        case -1:
            return avl_lower(tree->lt, x, best);
            break;
        case 0:
            return tree;
            break;
        case 1:
            return avl_lower(tree->rt, x, pmax(tree, best));
            break;
        }
    }
}

void rotate_right(Node * tree) {
    Node * child = tree->lt;
    Node * gamma = tree->rt;

    Node * alfa  = child->lt;
    Node * beta  = child->rt;

    long long int tmp;

    tmp = child->x;
    child->x = tree->x;
    tree->x = tmp;

    child->lt = beta;
    child->rt = gamma;
    child->height = max1(get_height(beta), get_height(gamma));

    tree->lt = alfa;
    tree->rt = child;
    tree->height = max1(get_height(alfa), get_height(child));
}

int max1(int x, int y) {
    return (x >= y ? x : y) + 1;
}

void rotate_left(Node * tree) {
    Node * alfa   = tree->lt;
    Node * child  = tree->rt;

    Node * beta   = child->lt;
    Node * gamma  = child->rt;

    long long int tmp;

    tmp = child->x;
    child->x = tree->x;
    tree->x = tmp;

    child->lt = alfa;
    child->rt = beta;
    child->height = max1(get_height(alfa), get_height(beta));

    tree->lt = child;
    tree->rt = gamma;
    tree->height = max1(get_height(child), get_height(gamma));
}

void avl_insert(Node ** tree_ptr, long long int x) {
    Node * tree = *tree_ptr;
    //printf("Bede wykonywal dodawanie %lld na %p\n", x, *tree_ptr);

    if (avl_is_empty(tree)) {
        Node * new_element;
        new_element = (Node *) malloc(sizeof(Node));

        new_element->height = 1;
        new_element->x = x;
        new_element->lt = avl_empty();
        new_element->rt = avl_empty();

        *tree_ptr = new_element;
    } else {
        switch(compare(x, tree->x)) {
        case -1:
            avl_insert(&(tree->lt), x);
            tree->height = max1(get_height(tree->lt), get_height(tree->rt));
            avl_balance(tree);
            break;
        case 0:
            break;
        case 1:
            avl_insert(&(tree->rt), x);
            tree->height = max1(get_height(tree->lt), get_height(tree->rt));
            avl_balance(tree);
            break;
        }
    }
}

void avl_balance(Node * tree) {
    switch(get_height(tree->lt) - get_height(tree->rt)) {
    case -2:
        avl_balance_right(tree);
        break;
    case 2:
        avl_balance_left(tree);
        break;
    case -1: case 0: case 1:
        break;
    }
}

void avl_balance_right(Node * tree) {
    switch (get_height(tree->rt->lt) - get_height(tree->rt->rt)) {
    case 1:
        rotate_right(tree->rt);
        tree->height = max1(get_height(tree->lt),get_height(tree->rt));
        rotate_left(tree);
        break;
    case -1: case 0:
        rotate_left(tree);
        break;
    }
}

void avl_balance_left(Node * tree) {
    switch (get_height(tree->lt->lt) - get_height(tree->lt->rt)) {
    case -1:
        rotate_left(tree->lt);
        tree->height = max1(get_height(tree->lt),get_height(tree->rt));
        rotate_right(tree);
        break;
    case 1: case 0:
        rotate_right(tree);
        break;
    }
}
